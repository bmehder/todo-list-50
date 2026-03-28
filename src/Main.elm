module Main exposing (main)

import Filter exposing (allFilters, applyFilter, filterToString)
import Html exposing (Html, button, div, form, input, label, li, menu, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, id, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode
import NonEmptyString
import NonNegative
import Text exposing (completedLabel, importantLabel, itemsLabel, remainingLabel)
import TimeTravel
import TimeTravelConfig
import Types exposing (..)
import Utils exposing (applyIf)



-- MODEL
-------------------------------------------------------------------------------


initModel : Model
initModel =
    { todos =
        [ { id = idFromIntUnsafe 0, task = taskFromStringUnsafe "Buy coffee", status = Active, important = False }
        , { id = idFromIntUnsafe 1, task = taskFromStringUnsafe "Write a 'not so small anymore' Elm app", status = Completed, important = False }
        , { id = idFromIntUnsafe 2, task = taskFromStringUnsafe "Profit", status = Active, important = False }
        , { id = idFromIntUnsafe 3, task = taskFromStringUnsafe "Do something important", status = Active, important = True }
        ]
    , draft = ""
    , filter = All
    , editing = NotEditing
    , pendingDelete = Nothing
    }



-- UPDATE
-------------------------------------------------------------------------------


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggledStatus id ->
            { model | todos = toggleStatusById id model.todos }

        ToggledImportant id ->
            { model | todos = toggleImportantById id model.todos }

        AskedToDelete id ->
            { model | pendingDelete = Just id }

        ConfirmedDelete id ->
            { model
                | todos = deleteTodoById id model.todos
                , pendingDelete = Nothing
            }

        CanceledDelete ->
            { model | pendingDelete = Nothing }

        UpdatedDraft newValue ->
            { model | draft = newValue }

        SetFilter newFilter ->
            { model | filter = newFilter }

        CreatedTodo ->
            createTodoFromDraft model

        StartedEditingTask id task ->
            { model | editing = EditingTask { id = id, draft = task } }

        UpdatedEditingDraft newValue ->
            case model.editing of
                EditingTask { id } ->
                    { model | editing = EditingTask { id = id, draft = newValue } }

                NotEditing ->
                    model

        SavedEditedTask ->
            case model.editing of
                EditingTask { id, draft } ->
                    case NonEmptyString.fromString draft of
                        Just task ->
                            { model
                                | todos = setTaskById id task model.todos
                                , editing = NotEditing
                            }

                        Nothing ->
                            { model | editing = NotEditing }

                NotEditing ->
                    model

        CanceledEdit ->
            { model | editing = NotEditing }

        NoOp ->
            model



-- DOMAIN HELPERS (Todo Logic)
-------------------------------------------------------------------------------


taskFromStringUnsafe : String -> Task
taskFromStringUnsafe str =
    case NonEmptyString.fromString str of
        Just task ->
            task

        Nothing ->
            Debug.todo "Invalid task literal"


idFromIntUnsafe : Int -> Id
idFromIntUnsafe n =
    case NonNegative.fromInt n of
        Just id ->
            id

        Nothing ->
            Debug.todo "Invalid Id literal"


nextId : List Todo -> Id
nextId todos =
    let
        maybeMaxId =
            todos
                |> List.map (.id >> NonNegative.toInt)
                |> List.maximum
    in
    case maybeMaxId of
        Just maxId ->
            idFromIntUnsafe <| maxId + 1

        Nothing ->
            idFromIntUnsafe 0


todoHasId : Id -> Todo -> Bool
todoHasId id todo =
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


toggleImportant : Todo -> Todo
toggleImportant todo =
    { todo | important = not todo.important }


toggleImportantById : Id -> List Todo -> List Todo
toggleImportantById id =
    List.map (applyIf (todoHasId id) toggleImportant)


setTask : Task -> Todo -> Todo
setTask newTask todo =
    { todo | task = newTask }


toggleStatusById : Id -> List Todo -> List Todo
toggleStatusById id =
    List.map (applyIf (todoHasId id) toggleStatus)


setTaskById : Id -> Task -> List Todo -> List Todo
setTaskById id newTask =
    List.map (applyIf (todoHasId id) (setTask newTask))


deleteTodoById : Id -> List Todo -> List Todo
deleteTodoById id =
    List.filter (not << todoHasId id)


createTodoFromDraft : Model -> Model
createTodoFromDraft model =
    case NonEmptyString.fromString model.draft of
        Just task ->
            let
                newTodo =
                    { id = nextId model.todos
                    , task = task
                    , status = Active
                    , important = False
                    }
            in
            { model
                | todos = model.todos ++ [ newTodo ]
                , draft = ""
            }

        Nothing ->
            model



-- VIEW
-------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [ class "flow" ]
        [ viewNewTodoForm model
        , viewFilterButtons model
        , viewTodos model
        , viewTodosCount model
        , viewConfirmDialog model
        ]


viewNewTodoForm : Model -> Html Msg
viewNewTodoForm model =
    form
        [ class "grid gap-1"
        , onSubmit CreatedTodo
        ]
        [ input
            [ value model.draft
            , onInput UpdatedDraft
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
    menu [ class "grid grid-template-columns-4 gap-1 padding-0" ] <|
        List.map (viewFilterButton model.filter) allFilters


viewFilterButton : Filter -> Filter -> Html Msg
viewFilterButton current value =
    let
        label =
            filterToString value

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
    ul [ class "flow padding-0" ]
        (model.todos
            |> applyFilter model.filter
            |> List.map (viewTodo model)
        )


viewTodo : Model -> Todo -> Html Msg
viewTodo model todo =
    let
        isEditingThis =
            case model.editing of
                EditingTask { id } ->
                    todoHasId id todo

                NotEditing ->
                    False
    in
    li [ class "flex align-items-center gap-1" ]
        ([ input
            ([ type_ "checkbox"
             , checked (todo.status == Completed)
             , onCheck (\_ -> ToggledStatus todo.id)
             , class "cursor-pointer user-select-none"
             ]
                ++ (if isEditingThis then
                        [ Html.Attributes.style "visibility" "hidden" ]

                    else
                        []
                   )
            )
            []
         , viewTask model todo
         ]
            ++ (if isEditingThis then
                    []

                else
                    [ viewDeleteButton todo ]
               )
        )


viewTask : Model -> Todo -> Html Msg
viewTask model todo =
    case model.editing of
        EditingTask { id, draft } ->
            if todoHasId id todo then
                viewEditing draft

            else
                viewTaskStatus todo

        NotEditing ->
            viewTaskStatus todo


viewEditing : String -> Html Msg
viewEditing draft =
    div [ class "flex align-items-center gap-1" ]
        [ input
            [ Html.Attributes.id "editing-input"
            , stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
            , value draft
            , onInput UpdatedEditingDraft
            , stopPropagationOn "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\key ->
                            if key == "Enter" then
                                Decode.succeed ( SavedEditedTask, True )

                            else if key == "Escape" then
                                Decode.succeed ( CanceledEdit, True )

                            else
                                Decode.fail "ignore"
                        )
                )
            ]
            []
        , button
            [ onClick SavedEditedTask
            , class "save-btn"
            , disabled (not <| NonEmptyString.isValid draft)
            ]
            [ text "Save" ]
        , button
            [ onClick CanceledEdit
            , class "cancel-btn"
            ]
            [ text "Cancel" ]
        ]


viewTaskStatus : Todo -> Html Msg
viewTaskStatus todo =
    let
        statusClass =
            let
                base =
                    case todo.status of
                        Active ->
                            "cursor-pointer"

                        Completed ->
                            "cursor-pointer line-through opacity-60"
            in
            if todo.important then
                base ++ " text-warning"

            else
                base
    in
    span
        [ class (statusClass ++ " user-select-none")
        , preventDefaultOn "mousedown" (Decode.succeed ( NoOp, True ))
        , stopPropagationOn "click"
            (Decode.field "shiftKey" Decode.bool
                |> Decode.andThen
                    (\isShift ->
                        if isShift then
                            Decode.succeed ( ToggledImportant todo.id, True )

                        else
                            Decode.succeed
                                ( StartedEditingTask
                                    todo.id
                                    (NonEmptyString.toString todo.task)
                                , True
                                )
                    )
            )
        ]
        [ text (NonEmptyString.toString todo.task) ]


viewDeleteButton : Todo -> Html Msg
viewDeleteButton todo =
    button
        [ stopPropagationOn "click" (Decode.succeed ( AskedToDelete todo.id, True ))
        , class "delete-btn delete-task cursor-pointer"
        ]
        [ text "✕" ]


viewTodosCount : Model -> Html Msg
viewTodosCount model =
    let
        count =
            model.todos
                |> applyFilter model.filter
                |> List.length

        labelForFilter =
            case model.filter of
                All ->
                    itemsLabel

                ActiveOrImportantOnly ->
                    remainingLabel

                CompletedOnly ->
                    completedLabel

                ImportantOnly ->
                    importantLabel
    in
    div [ class "text-align-center flow" ]
        [ div [ class "opacity-60 font-size-small" ]
            [ text "Tip: Click to edit • Shift-click to mark important" ]
        , div []
            [ text (String.fromInt count ++ labelForFilter count) ]
        ]


viewConfirmDialog : Model -> Html Msg
viewConfirmDialog model =
    case model.pendingDelete of
        Just id ->
            div [ class "confirm-dialog flex align-items-center gap-1" ]
                [ text "Delete this todo?"
                , button [ class "delete-btn", onClick (ConfirmedDelete id) ] [ text "Yes" ]
                , button [ onClick CanceledDelete ] [ text "Cancel" ]
                ]

        Nothing ->
            text ""



-- PROGRAM
-------------------------------------------------------------------------------


main : Program TimeTravel.Flags (TimeTravel.TimeTravel Msg Model) (TimeTravel.Msg Msg)
main =
    TimeTravel.withTimeTravel
        { init = initModel
        , update = update
        , view = view
        , msgToDebug = TimeTravelConfig.todoMsgToDebug
        , modelToString = TimeTravelConfig.modelToPrettyString
        }
