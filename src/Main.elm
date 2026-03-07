module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, input, li, menu, span, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, type_, value)
import Html.Events exposing (on, onBlur, onClick, onDoubleClick, onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode



-- MODEL


type alias Id =
    Int


type alias Draft =
    String


type Status
    = Active
    | Completed


type Filter
    = All
    | ActiveOnly
    | CompletedOnly


type alias Todo =
    { id : Id
    , task : String
    , status : Status
    }


type alias Model =
    { todos : List Todo
    , nextId : Id
    , draft : Draft
    , filter : Filter
    , editId : Maybe Id
    , editDraft : Draft
    }


init : Model
init =
    { todos =
        [ { id = 0, task = "Buy coffee", status = Active }
        , { id = 1, task = "Write the smallest Elm app", status = Completed }
        , { id = 2, task = "Profit", status = Active }
        ]
    , nextId = 3
    , draft = ""
    , filter = All
    , editId = Nothing
    , editDraft = ""
    }



-- UPDATE


type Msg
    = ToggleTodo Id
    | DeleteTodo Id
    | UpdateDraft Draft
    | SetFilter Filter
    | CreateTodo
    | StartEditing Id
    | UpdateEditDraft Draft
    | SaveEdit
    | CancelEdit
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleTodo id ->
            { model
                | todos = toggleTodoById id model.todos
            }

        DeleteTodo id ->
            { model
                | todos = deleteTodoById id model.todos
            }

        UpdateDraft newValue ->
            { model | draft = newValue }

        SetFilter newFilter ->
            { model | filter = newFilter }

        CreateTodo ->
            createIfValid model

        StartEditing id ->
            case findTodoById id model.todos of
                Just todo ->
                    { model
                        | editId = Just id
                        , editDraft = todo.task
                    }

                Nothing ->
                    model

        UpdateEditDraft newValue ->
            { model | editDraft = newValue }

        SaveEdit ->
            let
                updatedTodos =
                    List.map
                        (\todo ->
                            if Just todo.id == model.editId then
                                { todo | task = model.editDraft }

                            else
                                todo
                        )
                        model.todos
            in
            { model
                | todos = updatedTodos
                , editId = Nothing
                , editDraft = ""
            }

        CancelEdit ->
            { model | editId = Nothing, editDraft = "" }

        NoOp ->
            model



-- DOMAIN HELPERS


toggleStatus : Todo -> Todo
toggleStatus todo =
    { todo
        | status =
            case todo.status of
                Active ->
                    Completed

                Completed ->
                    Active
    }


matchesTodoId : Id -> Todo -> Bool
matchesTodoId id todo =
    id == todo.id


findTodoById : Id -> List Todo -> Maybe Todo
findTodoById id todos =
    List.filter (matchesTodoId id) todos
        |> List.head


toggleTodoById : Id -> List Todo -> List Todo
toggleTodoById id =
    List.map (toggleIfMatch id)


deleteTodoById : Id -> List Todo -> List Todo
deleteTodoById id =
    List.filter (not << matchesTodoId id)


toggleIfMatch : Id -> Todo -> Todo
toggleIfMatch id todo =
    if matchesTodoId id todo then
        toggleStatus todo

    else
        todo


createIfValid : Model -> Model
createIfValid model =
    if String.trim model.draft == "" then
        model

    else
        let
            newTodo =
                { id = model.nextId
                , task = model.draft
                , status = Active
                }
        in
        { model
            | todos = model.todos ++ [ newTodo ]
            , nextId = model.nextId + 1
            , draft = ""
        }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flow", onClick CancelEdit ]
        [ h1 [] [ text "Todo List" ]
        , viewNewTodo model
        , viewFilters model.filter
        , ul [ class "flow" ]
            (model.todos
                |> filterTodos model.filter
                |> List.map (viewTodo model)
            )
        , div [ class "text-align-center" ] [ viewFilteredCount model ]
        ]


viewNewTodo : Model -> Html Msg
viewNewTodo model =
    form
        [ class "grid gap-1"
        , onSubmit CreateTodo
        ]
        [ input
            [ value model.draft
            , onInput UpdateDraft
            , placeholder "Add a todo..."
            ]
            []
        , button
            [ type_ "submit"
            , disabled (String.trim model.draft == "")
            ]
            [ text "Add" ]
        ]


viewFilters : Filter -> Html Msg
viewFilters filter =
    menu [ class "grid grid-template-columns-3 gap-1" ]
        [ viewFilterButton "All" All filter
        , viewFilterButton "Active" ActiveOnly filter
        , viewFilterButton "Completed" CompletedOnly filter
        ]


viewFilterButton : String -> Filter -> Filter -> Html Msg
viewFilterButton label filterValue currentFilter =
    let
        isSelected =
            filterValue == currentFilter

        buttonClass =
            if isSelected then
                "filter-btn selected-btn"

            else
                "filter-btn"
    in
    li [ class "grid list-style-none" ]
        [ button
            [ class buttonClass
            , onClick (SetFilter filterValue)
            ]
            [ text label ]
        ]


viewTodo : Model -> Todo -> Html Msg
viewTodo model todo =
    li [ class "flex space-between align-items-center gap-1", onDoubleClick (StartEditing todo.id) ]
        [ viewItem model todo
        , viewDeleteButton todo
        ]


viewItem : Model -> Todo -> Html Msg
viewItem model todo =
    if model.editId == Just todo.id then
        input
            [ stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
            , value model.editDraft
            , onInput UpdateEditDraft
            , onBlur SaveEdit
            , on "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\key ->
                            if key == "Enter" then
                                Decode.succeed SaveEdit

                            else if key == "Escape" then
                                Decode.succeed CancelEdit

                            else
                                Decode.fail "ignore"
                        )
                )
            ]
            []

    else
        case todo.status of
            Active ->
                span
                    [ class "cursor-pointer", onClick (ToggleTodo todo.id) ]
                    [ text todo.task ]

            Completed ->
                span
                    [ class "cursor-pointer line-through opacity-60", onClick (ToggleTodo todo.id) ]
                    [ text todo.task ]


viewDeleteButton : Todo -> Html Msg
viewDeleteButton todo =
    button
        [ stopPropagationOn "click" (Decode.succeed ( DeleteTodo todo.id, True ))
        , class "delete-btn cursor-pointer"
        ]
        [ text "✕" ]


viewFilteredCount : Model -> Html Msg
viewFilteredCount model =
    let
        visibleTodos =
            filterTodos model.filter model.todos

        count =
            List.length visibleTodos

        label =
            case model.filter of
                All ->
                    if count == 1 then
                        " item"

                    else
                        " items"

                ActiveOnly ->
                    if count == 1 then
                        " item remaining"

                    else
                        " items remaining"

                CompletedOnly ->
                    if count == 1 then
                        " item completed"

                    else
                        " items completed"
    in
    text (String.fromInt count ++ label)


filterTodos : Filter -> List Todo -> List Todo
filterTodos filterType todos =
    case filterType of
        All ->
            todos

        ActiveOnly ->
            List.filter (.status >> (==) Active) todos

        CompletedOnly ->
            List.filter (.status >> (==) Completed) todos



-- PROGRAM


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
