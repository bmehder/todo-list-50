module Main exposing (main)

import Filter exposing (allFilters, applyFilter, filterToString)
import Html exposing (Html, button, div, form, hr, input, label, li, menu, span, text, ul)
import Html.Attributes exposing (attribute, checked, class, disabled, id, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode
import LabelText exposing (completedLabel, importantLabel, itemsLabel, remainingLabel)
import NonEmptyString
import NonNegative
import TimeTravel
import TimeTravelConfig
import Types exposing (..)
import Utils exposing (applyIf)



-- MODEL
-------------------------------------------------------------------------------


initModel : Model
initModel =
    { todos =
        [ { id = idFromIntUnsafe 0, todoText = todoTextFromStringUnsafe "Buy coffee", status = Active, important = False }
        , { id = idFromIntUnsafe 1, todoText = todoTextFromStringUnsafe "Write a small app in Elm", status = Completed, important = False }
        , { id = idFromIntUnsafe 2, todoText = todoTextFromStringUnsafe "Profit", status = Active, important = False }
        , { id = idFromIntUnsafe 3, todoText = todoTextFromStringUnsafe "Do something important", status = Active, important = True }
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

        StartedEditingTodoText id todoText ->
            { model | editing = EditingTodoText { id = id, draft = todoText } }

        UpdatedEditingDraft newValue ->
            case model.editing of
                EditingTodoText { id } ->
                    { model | editing = EditingTodoText { id = id, draft = newValue } }

                NotEditing ->
                    model

        SavedEditedTodoText ->
            case model.editing of
                EditingTodoText { id, draft } ->
                    case NonEmptyString.fromString draft of
                        Just todoText ->
                            { model
                                | todos = setTodoTextById id todoText model.todos
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


todoTextFromStringUnsafe : String -> TodoText
todoTextFromStringUnsafe str =
    case NonEmptyString.fromString str of
        Just todoText ->
            todoText

        Nothing ->
            Debug.todo "Invalid TodoText literal"


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
todoHasId id =
    .id >> (==) id


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


setTodoText : TodoText -> Todo -> Todo
setTodoText newTodoText todo =
    { todo | todoText = newTodoText }


toggleStatusById : Id -> List Todo -> List Todo
toggleStatusById id =
    List.map (applyIf (todoHasId id) toggleStatus)


setTodoTextById : Id -> TodoText -> List Todo -> List Todo
setTodoTextById id newTodoText =
    List.map (applyIf (todoHasId id) (setTodoText newTodoText))


deleteTodoById : Id -> List Todo -> List Todo
deleteTodoById id =
    List.filter (not << todoHasId id)


createTodoFromDraft : Model -> Model
createTodoFromDraft model =
    case NonEmptyString.fromString model.draft of
        Just todoText ->
            let
                newTodo =
                    { id = nextId model.todos
                    , todoText = todoText
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
        , hr [] []
        ]


viewNewTodoForm : Model -> Html Msg
viewNewTodoForm model =
    form
        [ class "grid gap-1"
        , onSubmit CreatedTodo
        ]
        [ input
            [ type_ "search"
            , value model.draft
            , onInput UpdatedDraft
            , placeholder "Add a todo..."
            , attribute "aria-label" "Add a new todo"
            , attribute "role" "textbox"
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
                EditingTodoText { id } ->
                    todoHasId id todo

                NotEditing ->
                    False
    in
    li [ class "flex align-items-center gap-1" ]
        ([ input
            ([ type_ "checkbox"
             , checked (todo.status == Completed)
             , onCheck (\_ -> ToggledStatus todo.id)
             , class "flex-shrink-0 cursor-pointer user-select-none"
             , Html.Attributes.attribute "aria-label"
                ("Mark todo as completed: " ++ NonEmptyString.toString todo.todoText)
             ]
                ++ (if isEditingThis then
                        [ Html.Attributes.style "visibility" "hidden" ]

                    else
                        []
                   )
            )
            []
         , viewTodoText model todo
         ]
            ++ (if isEditingThis then
                    []

                else
                    case model.pendingDelete of
                        Just id ->
                            if todoHasId id todo then
                                [ viewConfirmInline todo ]

                            else
                                [ viewDeleteButton todo ]

                        Nothing ->
                            [ viewDeleteButton todo ]
               )
        )


viewTodoText : Model -> Todo -> Html Msg
viewTodoText model todo =
    case model.editing of
        EditingTodoText { id, draft } ->
            if todoHasId id todo then
                viewEditing draft

            else
                viewTodoStatus todo

        NotEditing ->
            viewTodoStatus todo


viewEditing : String -> Html Msg
viewEditing draft =
    div [ class "flex align-items-center gap-1" ]
        [ input
            [ Html.Attributes.id "editing-input"
            , Html.Attributes.attribute "aria-label" "Edit todo"
            , stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
            , value draft
            , onInput UpdatedEditingDraft
            , stopPropagationOn "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\key ->
                            if key == "Enter" then
                                Decode.succeed ( SavedEditedTodoText, True )

                            else if key == "Escape" then
                                Decode.succeed ( CanceledEdit, True )

                            else
                                Decode.fail "ignore"
                        )
                )
            ]
            []
        , button
            [ onClick SavedEditedTodoText
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


viewTodoStatus : Todo -> Html Msg
viewTodoStatus todo =
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
        , Html.Attributes.tabindex 0
        , Html.Attributes.attribute "role" "button"
        , Html.Attributes.attribute "aria-label"
            ("Edit todo " ++ NonEmptyString.toString todo.todoText)
        , preventDefaultOn "mousedown" (Decode.succeed ( NoOp, True ))
        , stopPropagationOn "click"
            (Decode.field "shiftKey" Decode.bool
                |> Decode.andThen
                    (\isShift ->
                        if isShift then
                            Decode.succeed ( ToggledImportant todo.id, True )

                        else
                            Decode.succeed
                                ( StartedEditingTodoText
                                    todo.id
                                    (NonEmptyString.toString todo.todoText)
                                , True
                                )
                    )
            )
        ]
        [ text (NonEmptyString.toString todo.todoText) ]


viewDeleteButton : Todo -> Html Msg
viewDeleteButton todo =
    button
        [ stopPropagationOn "click" (Decode.succeed ( AskedToDelete todo.id, True ))
        , class "delete-btn delete-task cursor-pointer"
        , Html.Attributes.attribute "aria-label"
            ("Delete todo " ++ NonEmptyString.toString todo.todoText)
        ]
        [ text "✕" ]


viewConfirmInline : Todo -> Html Msg
viewConfirmInline todo =
    div
        [ class "flex align-items-center gap-1 confirm-inline"
        , Html.Attributes.attribute "role" "group"
        , Html.Attributes.attribute "aria-label" "Confirm delete"
        ]
        [ text "Delete?"
        , button [ class "delete-btn", onClick (ConfirmedDelete todo.id) ] [ text "Yes" ]
        , button [ onClick CanceledDelete ] [ text "Cancel" ]
        ]


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
        , decodeMsg = TimeTravelConfig.decodeMsg
        }
