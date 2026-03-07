module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, input, li, menu, span, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode



-- MODEL


type alias Id =
    Int


type Status
    = Active
    | Completed


type Filter
    = All
    | ActiveOnly
    | CompletedOnly


type alias Todo =
    { id : Id
    , item : String
    , status : Status
    }


type alias Model =
    { todos : List Todo
    , nextId : Id
    , draft : String
    , filter : Filter
    }


init : Model
init =
    { todos =
        [ { id = 0, item = "Buy coffee", status = Active }
        , { id = 1, item = "Write the smallest Elm app", status = Completed }
        , { id = 2, item = "Profit", status = Active }
        ]
    , nextId = 3
    , draft = ""
    , filter = All
    }



-- UPDATE


type Msg
    = ToggleTodo Id
    | DeleteTodo Id
    | UpdateDraft String
    | SetFilter Filter
    | CreateTodo


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleTodo id ->
            { model
                | todos = toggleById id <| model.todos
            }

        DeleteTodo id ->
            { model
                | todos = deleteById id <| model.todos
            }

        UpdateDraft newValue ->
            { model | draft = newValue }

        SetFilter newFilter ->
            { model | filter = newFilter }

        CreateTodo ->
            createIfValid model


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

matchesId : Id -> Todo -> Bool
matchesId id todo =
    todo.id == id


toggleIfMatch : Id -> Todo -> Todo
toggleIfMatch id todo =
    if matchesId id todo then
        toggleStatus todo

    else
        todo


toggleById : Id -> List Todo -> List Todo
toggleById id =
    List.map (toggleIfMatch id)


deleteById : Id -> List Todo -> List Todo
deleteById id =
    List.filter (not << matchesId id)


createIfValid : Model -> Model
createIfValid model =
    if String.trim model.draft == "" then
        model

    else
        let
            newTodo =
                { id = model.nextId
                , item = model.draft
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
    div [ class "flow" ]
        [ h1 [] [ text "Todo List" ]
        , viewCreate model
        , ul [ class "flow" ]
            (model.todos
                |> filterTodos model.filter
                |> List.map viewTodo
            )
        , viewFilters model.filter
        , div [ class "text-align-center" ] [ viewFilteredCount model ]
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
    li [ class "grid list-style-none"]
        [ button
            [ class buttonClass
            , onClick (SetFilter filterValue)
            ]
            [ text label ]
        ]


viewFilters : Filter -> Html Msg
viewFilters filter =
    menu [ class "grid grid-template-columns-3 gap-1" ]
        [ viewFilterButton "All" All filter
        , viewFilterButton "Active" ActiveOnly filter
        , viewFilterButton "Completed" CompletedOnly filter
        ]



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


viewCreate : Model -> Html Msg
viewCreate model =
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


viewTodo : Todo -> Html Msg
viewTodo todo =
    li [ class "flex space-between align-items-center gap-1", onClick (ToggleTodo todo.id) ]
        [ viewItem todo
        , viewDeleteButton todo
        ]


viewItem : Todo -> Html Msg
viewItem todo =
    case todo.status of
        Active ->
            span
                [ class "cursor-pointer" ]
                [ text todo.item ]

        Completed ->
            span
                [ class "cursor-pointer line-through opacity-60" ]
                [ text todo.item ]


viewDeleteButton : Todo -> Html Msg
viewDeleteButton todo =
    button
        [ stopPropagationOn "click" (Decode.succeed ( DeleteTodo todo.id, True ))
        , class "delete-btn cursor-pointer"
        ]
        [ text "✕" ]


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
