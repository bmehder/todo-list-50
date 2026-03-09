module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, li, menu, span, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, type_, value)
import Html.Events exposing (on, onBlur, onClick, onDoubleClick, onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode



-- MODEL


type alias Id =
    Int


type alias Task =
    String


type Status
    = Active
    | Completed


type Editing
    = NotEditing
    | Editing Id Task


type Filter
    = All
    | ActiveOnly
    | CompletedOnly


allFilters : List Filter
allFilters =
    [ All, ActiveOnly, CompletedOnly ]


type alias Todo =
    { id : Id
    , task : Task
    , status : Status
    }


type alias Model =
    { todos : List Todo
    , draft : Task
    , filter : Filter
    , editing : Editing
    }


init : Model
init =
    { todos =
        [ { id = 0, task = "Buy coffee", status = Active }
        , { id = 1, task = "Write the smallest Elm app", status = Completed }
        , { id = 2, task = "Profit", status = Active }
        ]
    , draft = ""
    , filter = All
    , editing = NotEditing
    }



-- UPDATE


type Msg
    = ToggleTodo Id
    | DeleteTodo Id
    | UpdateDraft Task
    | SetFilter Filter
    | CreateTodo
    | StartEditing Id
    | UpdateEditDraft Task
    | SaveEdit
    | CancelEdit
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleTodo id ->
            { model
                | todos = toggleTodoById id <| model.todos
            }

        DeleteTodo id ->
            { model
                | todos = deleteTodoById id <| model.todos
            }

        UpdateDraft newValue ->
            { model | draft = newValue }

        SetFilter newFilter ->
            { model | filter = newFilter }

        CreateTodo ->
            createIfValid model

        StartEditing id ->
            case findTodoById id <| model.todos of
                Just todo ->
                    { model
                        | editing = Editing id todo.task
                    }

                Nothing ->
                    model

        UpdateEditDraft newValue ->
            case model.editing of
                Editing id _ ->
                    { model | editing = Editing id newValue }

                NotEditing ->
                    model

        SaveEdit ->
            case model.editing of
                Editing id draft ->
                    { model
                        | todos = updateTaskById id draft model.todos
                        , editing = NotEditing
                    }

                NotEditing ->
                    model

        CancelEdit ->
            { model | editing = NotEditing }

        NoOp ->
            model



-- UTILITY FUNCTIONS


applyIf : (a -> Bool) -> (a -> a) -> a -> a
applyIf predicate transform value =
    if predicate value then
        transform value

    else
        value



-- DOMAIN HELPERS


nextId : List Todo -> Id
nextId =
    List.map .id
        >> List.maximum
        >> Maybe.map ((+) 1)
        >> Maybe.withDefault 0


matchesTodoId : Id -> Todo -> Bool
matchesTodoId id todo =
    id == todo.id


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


updateTask : Task -> Todo -> Todo
updateTask newTask todo =
    { todo | task = newTask }


toggleTodoById : Id -> List Todo -> List Todo
toggleTodoById id =
    List.map (applyIf (matchesTodoId id) toggleStatus)


updateTaskById : Id -> Task -> List Todo -> List Todo
updateTaskById id newTask =
    List.map (applyIf (matchesTodoId id) (updateTask newTask))


deleteTodoById : Id -> List Todo -> List Todo
deleteTodoById id =
    List.filter (not << matchesTodoId id)


findTodoById : Id -> List Todo -> Maybe Todo
findTodoById id =
    List.filter (matchesTodoId id)
        >> List.head


filterTodos : Filter -> List Todo -> List Todo
filterTodos filter =
    case filter of
        All ->
            identity

        ActiveOnly ->
            List.filter (.status >> (==) Active)

        CompletedOnly ->
            List.filter (.status >> (==) Completed)


createIfValid : Model -> Model
createIfValid model =
    if
        model.draft
            |> String.trim
            |> String.isEmpty
    then
        model

    else
        let
            newTodo =
                [ { id = nextId model.todos
                  , task = model.draft
                  , status = Active
                  }
                ]
        in
        { model
            | todos = model.todos ++ newTodo
            , draft = ""
        }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flow", onClick CancelEdit ]
        [ viewNewTodoForm model
        , viewFilterButtons model
        , viewTodos model
        , viewTodosCount model
        ]



-- VIEW HELPERS


viewNewTodoForm : Model -> Html Msg
viewNewTodoForm model =
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
            , disabled (model.draft |> String.trim |> String.isEmpty)
            ]
            [ text "Add" ]
        ]


viewFilterButtons : Model -> Html Msg
viewFilterButtons model =
    allFilters
        |> List.map (viewFilterButton model.filter)
        |> menu [ class "grid grid-template-columns-3 gap-1" ]


viewFilterButton : Filter -> Filter -> Html Msg
viewFilterButton current value =
    let
        label =
            case value of
                All ->
                    "All"

                ActiveOnly ->
                    "Active"

                CompletedOnly ->
                    "Completed"

        isSelected =
            value == current

        buttonClass =
            if isSelected then
                "filter-btn selected-btn"

            else
                "filter-btn"
    in
    li [ class "grid list-style-none" ]
        [ button
            [ class buttonClass
            , onClick (SetFilter value)
            ]
            [ text label ]
        ]


viewTodo : Model -> Todo -> Html Msg
viewTodo model todo =
    li [ class "flex space-between align-items-center gap-1", onDoubleClick (StartEditing todo.id) ]
        [ viewTask model todo
        , viewDeleteButton todo
        ]


viewTask : Model -> Todo -> Html Msg
viewTask model todo =
    case model.editing of
        Editing id draft ->
            if matchesTodoId id todo then
                viewEditing draft

            else
                viewTaskStatus todo

        NotEditing ->
            viewTaskStatus todo


viewEditing : Task -> Html Msg
viewEditing draft =
    input
        [ stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
        , value draft
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


viewTaskStatus : Todo -> Html Msg
viewTaskStatus todo =
    let
        statusClass =
            case todo.status of
                Active ->
                    "cursor-pointer"

                Completed ->
                    "cursor-pointer line-through opacity-60"
    in
    span
        [ class statusClass
        , onClick (ToggleTodo todo.id)
        ]
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
        count =
            model.todos
                |> filterTodos model.filter
                |> List.length

        labelForFilter =
            case model.filter of
                All ->
                    itemsLabel

                ActiveOnly ->
                    remainingLabel

                CompletedOnly ->
                    completedLabel
    in
    text (String.fromInt count ++ labelForFilter count)


viewTodos : Model -> Html Msg
viewTodos model =
    ul [ class "flow" ]
        (model.todos
            |> filterTodos model.filter
            |> List.map (viewTodo model)
        )


viewTodosCount : Model -> Html Msg
viewTodosCount model =
    div [ class "text-align-center" ] [ viewFilteredCount model ]



-- VIEW UTILITIES


itemsLabel : Int -> String
itemsLabel =
    pluralize " item" " items"


remainingLabel : Int -> String
remainingLabel =
    pluralize " item remaining" " items remaining"


completedLabel : Int -> String
completedLabel =
    pluralize " item completed" " items completed"


pluralize : String -> String -> Int -> String
pluralize singular plural count =
    if count == 1 then
        singular

    else
        plural



-- PROGRAM


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
