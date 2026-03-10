module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, li, menu, span, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, type_, value)
import Html.Events exposing (on, onBlur, onClick, onDoubleClick, onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode
import NonEmptyString exposing (NonEmptyString)
import NonNegative exposing (NonNegative)



-- MODEL


type alias Id =
    NonNegative


type alias Task =
    NonEmptyString


type Status
    = Active
    | Completed


type Editing
    = NotEditing
    | Editing Id String


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
    , draft : String
    , filter : Filter
    , editing : Editing
    }


init : Model
init =
    { todos =
        [ { id = unsafeId 0, task = unsafeTask "Buy coffee", status = Active }
        , { id = unsafeId 1, task = unsafeTask "Write the smallest Elm app", status = Completed }
        , { id = unsafeId 2, task = unsafeTask "Profit", status = Active }
        ]
    , draft = ""
    , filter = All
    , editing = NotEditing
    }



-- UPDATE


type Msg
    = ToggleTodoStatus Id
    | DeleteTodo Id
    | UpdateDraft String
    | SetFilter Filter
    | CreateTodo
    | StartEditing Id
    | UpdateEditDraft String
    | SaveEdit
    | CancelEdit
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleTodoStatus id ->
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
            addTodoFromDraft model

        StartEditing id ->
            case findTodoById id <| model.todos of
                Just todo ->
                    { model
                        | editing = Editing id (NonEmptyString.toString todo.task)
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
                    case NonEmptyString.fromString draft of
                        Just task ->
                            { model
                                | todos = updateTaskById id task model.todos
                                , editing = NotEditing
                            }

                        Nothing ->
                            { model | editing = NotEditing }

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


unsafeTask : String -> Task
unsafeTask str =
    case NonEmptyString.fromString str of
        Just task ->
            task

        Nothing ->
            Debug.todo "Invalid task literal"



-- idFromInt : Int -> Id
-- idFromInt n =
--     case NonNegative.fromInt n of
--         Just id ->
--             id
--         Nothing ->
--             Debug.todo "Invalid negative Id"



unsafeId : Int -> Id
unsafeId n =
    case NonNegative.fromInt n of
        Just id ->
            id

        Nothing ->
            Debug.todo "Invalid Id literal"


nextId : List Todo -> Id
nextId todos =
    todos
        |> List.map (.id >> NonNegative.toInt)
        |> List.maximum
        |> Maybe.withDefault -1
        |> (+) 1
        |> unsafeId


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


addTodoFromDraft : Model -> Model
addTodoFromDraft model =
    case NonEmptyString.fromString model.draft of
        Just task ->
            let
                newTodo =
                    { id = nextId model.todos
                    , task = task
                    , status = Active
                    }
            in
            { model
                | todos = model.todos ++ [ newTodo ]
                , draft = ""
            }

        Nothing ->
            model



-- VIEW
-- view
--  ─ viewNewTodoForm
--  ─ viewFilterButtons
--     ─ viewFilterButton
--  ─ viewTodos
--     ─ viewTodo
--         ─ viewTask
--            ─ viewEditing
--            ─ viewTaskStatus
--         ─ viewDeleteButton
--  ─ viewTodosCount
--      ─ viewFilteredCount


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
            , disabled (not <| NonEmptyString.isValid model.draft)
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


viewTodos : Model -> Html Msg
viewTodos model =
    ul [ class "flow" ]
        (model.todos
            |> filterTodos model.filter
            |> List.map (viewTodo model)
        )


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


viewEditing : String -> Html Msg
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
        , onClick (ToggleTodoStatus todo.id)
        ]
        [ text (NonEmptyString.toString todo.task) ]


viewDeleteButton : Todo -> Html Msg
viewDeleteButton todo =
    button
        [ stopPropagationOn "click" (Decode.succeed ( DeleteTodo todo.id, True ))
        , class "delete-btn cursor-pointer"
        ]
        [ text "✕" ]


viewTodosCount : Model -> Html Msg
viewTodosCount model =
    div [ class "text-align-center" ] [ viewFilteredCount model ]


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
