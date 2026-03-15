module Main exposing (main)

import Browser
import Html exposing (Html, button, details, div, form, h2, input, label, li, menu, section, span, summary, text, ul)
import Html.Attributes exposing (attribute, class, disabled, placeholder, type_, value)
import Html.Events exposing (on, onBlur, onCheck, onClick, onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode
import NonEmptyString exposing (NonEmptyString)
import NonNegative exposing (NonNegative)



-- MODEL
-------------------------------------------------------------------------------


type alias Id =
    NonNegative


type alias Task =
    NonEmptyString


type Status
    = Active
    | Completed
    | Important


type Editing
    = NotEditing
    | Editing Id String


type Filter
    = All
    | ActiveOnly
    | CompletedOnly
    | ImportantOnly


allFilters : List Filter
allFilters =
    [ All, ActiveOnly, CompletedOnly, ImportantOnly ]


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
    , pendingDelete : Maybe Id
    }


type alias Step =
    { msg : Msg
    , prev : Model
    , next : Model
    }


type alias Timeline =
    { past : List Step
    , present : Model
    , future : List Step
    }


type TimelineVisibility
    = TimelineHidden
    | TimelineVisible

type alias AppModel =
    { timeline : Timeline
    , timelineVisibility : TimelineVisibility
    }


initModel : Model
initModel =
    { todos =
        [ { id = unsafeId 0, task = unsafeTask "Buy coffee", status = Active }
        , { id = unsafeId 1, task = unsafeTask "Write the smallest Elm app", status = Completed }
        , { id = unsafeId 2, task = unsafeTask "Profit", status = Active }
        , { id = unsafeId 3, task = unsafeTask "Do something important", status = Important }
        ]
    , draft = ""
    , filter = All
    , editing = NotEditing
    , pendingDelete = Nothing
    }


init : AppModel
init =
    { timeline =
        { past = []
        , present = initModel
        , future = []
        }
    , timelineVisibility = TimelineHidden
    }



-- UPDATE
-------------------------------------------------------------------------------


type Msg
    = ToggleTodoStatus Id
    | AskToDelete Id
    | ConfirmDelete Id
    | CancelDelete
    | UpdateDraft String
    | SetFilter Filter
    | CreateTodo
    | StartEditing Id String
    | UpdateEditDraft String
    | SaveEdit
    | CancelEdit
    | NoOp
    | ToggleTimeline
    | AppMsg Msg
    | Prev
    | Next


updateTodo : Msg -> Model -> Model
updateTodo msg model =
    case msg of
        ToggleTodoStatus id ->
            { model
                | todos = toggleTodoById id model.todos
            }

        AskToDelete id ->
            { model | pendingDelete = Just id }

        ConfirmDelete id ->
            { model
                | todos = deleteTodoById id model.todos
                , pendingDelete = Nothing
            }

        CancelDelete ->
            { model | pendingDelete = Nothing }

        UpdateDraft newValue ->
            { model | draft = newValue }

        SetFilter newFilter ->
            { model | filter = newFilter }

        CreateTodo ->
            addTodoFromDraft model

        StartEditing id task ->
            { model | editing = Editing id task }

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

        ToggleTimeline ->
            model

        AppMsg _ ->
            model

        Prev ->
            model

        Next ->
            model


update : Msg -> AppModel -> AppModel
update msg app =
    case msg of
        AppMsg innerMsg ->
            let
                timeline =
                    app.timeline

                newModel =
                    updateTodo innerMsg timeline.present

                step =
                    { msg = innerMsg
                    , prev = timeline.present
                    , next = newModel
                    }

                newTimeline =
                    { past = step :: timeline.past
                    , present = newModel
                    , future = []
                    }
            in
            { app | timeline = newTimeline }

        Prev ->
            let
                timeline =
                    app.timeline
            in
            case timeline.past of
                step :: rest ->
                    { app
                        | timeline =
                            { past = rest
                            , present = step.prev
                            , future = step :: timeline.future
                            }
                    }

                [] ->
                    app

        Next ->
            let
                timeline =
                    app.timeline
            in
            case timeline.future of
                step :: rest ->
                    { app
                        | timeline =
                            { past = step :: timeline.past
                            , present = step.next
                            , future = rest
                            }
                    }

                [] ->
                    app

        ToggleTimeline ->
            { app
                | timelineVisibility =
                    case app.timelineVisibility of
                        TimelineHidden ->
                            TimelineVisible

                        TimelineVisible ->
                            TimelineHidden
            }

        _ ->
            update (AppMsg msg) app



-- UTILITY FUNCTIONS
-------------------------------------------------------------------------------


applyIf : (a -> Bool) -> (a -> a) -> a -> a
applyIf predicate transform value =
    if predicate value then
        transform value

    else
        value



-- DOMAIN HELPERS
-------------------------------------------------------------------------------


unsafeTask : String -> Task
unsafeTask str =
    case NonEmptyString.fromString str of
        Just task ->
            task

        Nothing ->
            Debug.todo "Invalid task literal"


unsafeId : Int -> Id
unsafeId n =
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
            unsafeId <| maxId + 1

        Nothing ->
            unsafeId 0


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
                    Important

                Important ->
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


filterTodos : Filter -> List Todo -> List Todo
filterTodos filterMode =
    case filterMode of
        All ->
            identity

        ActiveOnly ->
            List.filter (.status >> (==) Active)

        CompletedOnly ->
            List.filter (.status >> (==) Completed)

        ImportantOnly ->
            List.filter (.status >> (==) Important)


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
-------------------------------------------------------------------------------


view : AppModel -> Html Msg
view app =
    let
        timeline =
            app.timeline

        model =
            timeline.present
    in
    div [ class "flow" ]
        [ viewNewTodoForm model
        , viewFilterButtons model
        , viewTodos model
        , viewTodosCount model
        , viewConfirmDialog model
        , viewTimelineToggle app
        , if app.timelineVisibility == TimelineVisible then
            div [ class "timeline-wrapper flow" ]
                [ viewTimeline timeline
                , viewHistory timeline
                ]

          else
            text ""
        ]



-- VIEW HELPERS
-------------------------------------------------------------------------------


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
    menu [ class "grid grid-template-columns-4 gap-1 padding-0" ] <|
        List.map (viewFilterButton model.filter) allFilters


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

                ImportantOnly ->
                    "Important"

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
            |> filterTodos model.filter
            |> List.map (viewTodo model)
        )


viewTodo : Model -> Todo -> Html Msg
viewTodo model todo =
    li [ class "flex space-between align-items-center gap-1" ]
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

                Important ->
                    "cursor-pointer text-warning"
    in
    span
        [ class statusClass
        , stopPropagationOn "click"
            (Decode.field "shiftKey" Decode.bool
                |> Decode.andThen
                    (\isShift ->
                        if isShift then
                            Decode.succeed ( StartEditing todo.id (NonEmptyString.toString todo.task), True )

                        else
                            Decode.succeed ( ToggleTodoStatus todo.id, True )
                    )
            )
        ]
        [ text (NonEmptyString.toString todo.task) ]


viewDeleteButton : Todo -> Html Msg
viewDeleteButton todo =
    button
        [ stopPropagationOn "click" (Decode.succeed ( AskToDelete todo.id, True ))
        , class "delete-btn cursor-pointer"
        ]
        [ text "✕" ]


viewTodosCount : Model -> Html Msg
viewTodosCount model =
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

                ImportantOnly ->
                    importantLabel
    in
    div [ class "text-align-center flow" ]
        [ div []
            [ text (String.fromInt count ++ labelForFilter count) ]
        , div [ class "opacity-60 font-size-small" ]
            [ text "Tip: Shift-click a task to edit" ]
        ]


viewConfirmDialog : Model -> Html Msg
viewConfirmDialog model =
    model.pendingDelete
        |> Maybe.map
            (\id ->
                div [ class "confirm-dialog flex gap-1 align-items-center" ]
                    [ text "Delete this todo?"
                    , button [ onClick (ConfirmDelete id) ] [ text "Yes" ]
                    , button [ onClick CancelDelete ] [ text "Cancel" ]
                    ]
            )
        |> Maybe.withDefault (text "")



-- VIEW UTILITIES
-------------------------------------------------------------------------------


itemsLabel : Int -> String
itemsLabel =
    pluralize " item" " items"


remainingLabel : Int -> String
remainingLabel =
    pluralize " item remaining" " items remaining"


completedLabel : Int -> String
completedLabel =
    pluralize " item completed" " items completed"


importantLabel : Int -> String
importantLabel =
    pluralize " important item" " important items"


pluralize : String -> String -> Int -> String
pluralize singular plural count =
    case count of
        1 ->
            singular

        _ ->
            plural



-- TIME TRAVEL DEBUGGER
-------------------------------------------------------------------------------


viewModel : Model -> Html Msg
viewModel model =
    ul []
        [ li [] [ text ("draft: \"" ++ model.draft ++ "\"") ]
        , li [] [ text ("filter: " ++ filterToString model.filter) ]
        , li [] [ text ("editing: " ++ editingToString model.editing) ]
        , li [] [ text ("pendingDelete: " ++ pendingDeleteToString model.pendingDelete) ]
        , li []
            [ text "todos:"
            , ul [] (List.map viewTodoDebug model.todos)
            ]
        ]


viewTimelineToggle : AppModel -> Html Msg
viewTimelineToggle app =
    div [ class "flex gap-1 align-items-center" ]
        [ input
            [ type_ "checkbox"
            , Html.Attributes.checked (app.timelineVisibility == TimelineVisible)
            , onCheck (always ToggleTimeline)
            , attribute "id" "toggle-timeline"
            ]
            []
        , label
            [ attribute "for" "toggle-timeline" ]
            [ text "Show Time Travel Debugger" ]
        ]


viewTimeline : Timeline -> Html Msg
viewTimeline timeline =
    section [ class "timeline flow" ]
        [ h2 []
            [ text "Time Travel Debugger" ]
        , div [ class "flex gap-1 align-items-center" ]
            [ button
                [ onClick Prev
                , disabled (List.isEmpty timeline.past)
                ]
                [ text "Prev" ]
            , button
                [ onClick Next
                , disabled (List.isEmpty timeline.future)
                ]
                [ text "Next" ]
            , div []
                [ text ("Transitions: " ++ String.fromInt (List.length timeline.past)) ]
            ]
        ]


viewHistory : Timeline -> Html Msg
viewHistory timeline =
    ul [ class "flow padding-0 list-style-none" ]
        ((timeline.past
            |> List.map viewStep
         )
            ++ [ viewInitialStep timeline.present ]
        )


viewInitialStep : Model -> Html Msg
viewInitialStep model =
    li []
        [ details [ attribute "name" "timeline-step" ]
            [ summary []
                [ text "Msg: None (initial state)" ]
            , viewModel model
            ]
        ]


viewStep : Step -> Html Msg
viewStep step =
    li []
        [ details [ attribute "name" "timeline-step" ]
            [ summary []
                [ text ("Msg: " ++ msgToString step.msg) ]
            , div [ class "padding-inline-start-1-5" ]
                [ div [] [ text "Next Model:" ]
                , viewModelDiff step.prev step.next
                ]
            ]
        ]


viewModelDiff : Model -> Model -> Html Msg
viewModelDiff prev next =
    ul []
        [ viewField { name = "draft", prev = prev.draft, next = next.draft }
        , viewField { name = "filter", prev = filterToString prev.filter, next = filterToString next.filter }
        , viewField { name = "editing", prev = editingToString prev.editing, next = editingToString next.editing }
        , viewField { name = "pendingDelete", prev = pendingDeleteToString prev.pendingDelete, next = pendingDeleteToString next.pendingDelete }
        , li []
            [ text "todos:"
            , ul [] (List.map viewTodoDebug next.todos)
            ]
        ]


viewField : { name : String, prev : String, next : String } -> Html Msg
viewField field =
    if field.prev == field.next then
        li [] [ text (field.name ++ ": " ++ field.next) ]

    else
        li []
            [ span [ class "text-success" ]
                [ text (field.name ++ ": " ++ field.next) ]
            ]


viewTodoDebug : Todo -> Html Msg
viewTodoDebug todo =
    li []
        [ text "{"
        , ul []
            [ li [] [ text ("id: " ++ String.fromInt (NonNegative.toInt todo.id)) ]
            , li [] [ text ("status: " ++ statusToString todo.status) ]
            , li [] [ text ("task: \"" ++ NonEmptyString.toString todo.task ++ "\"") ]
            ]
        , text "}"
        ]



-- DEBUG STRING HELPERS
-------------------------------------------------------------------------------


msgToString : Msg -> String
msgToString msg =
    case msg of
        ToggleTodoStatus id ->
            "ToggleTodoStatus " ++ String.fromInt (NonNegative.toInt id)

        AskToDelete id ->
            "AskToDelete " ++ String.fromInt (NonNegative.toInt id)

        ConfirmDelete id ->
            "ConfirmDelete " ++ String.fromInt (NonNegative.toInt id)

        CancelDelete ->
            "CancelDelete"

        UpdateDraft str ->
            "UpdateDraft \"" ++ str ++ "\""

        SetFilter filter ->
            case filter of
                All ->
                    "SetFilter All"

                ActiveOnly ->
                    "SetFilter ActiveOnly"

                CompletedOnly ->
                    "SetFilter CompletedOnly"

                ImportantOnly ->
                    "SetFilter ImportantOnly"

        CreateTodo ->
            "CreateTodo"

        StartEditing id task ->
            "StartEditing "
                ++ String.fromInt (NonNegative.toInt id)
                ++ " \""
                ++ task
                ++ "\""

        UpdateEditDraft str ->
            "UpdateEditDraft \"" ++ str ++ "\""

        SaveEdit ->
            "SaveEdit"

        CancelEdit ->
            "CancelEdit"

        NoOp ->
            "NoOp"

        ToggleTimeline ->
            "ToggleTimeline"

        AppMsg inner ->
            "AppMsg (" ++ msgToString inner ++ ")"

        Prev ->
            "Prev"

        Next ->
            "Next"


filterToString : Filter -> String
filterToString filter =
    case filter of
        All ->
            "All"

        ActiveOnly ->
            "ActiveOnly"

        CompletedOnly ->
            "CompletedOnly"

        ImportantOnly ->
            "ImportantOnly"


editingToString : Editing -> String
editingToString editing =
    case editing of
        NotEditing ->
            "NotEditing"

        Editing id draft ->
            "Editing " ++ String.fromInt (NonNegative.toInt id) ++ " \"" ++ draft ++ "\""


pendingDeleteToString : Maybe Id -> String
pendingDeleteToString maybeId =
    case maybeId of
        Nothing ->
            "Nothing"

        Just id ->
            "Just " ++ String.fromInt (NonNegative.toInt id)


statusToString : Status -> String
statusToString status =
    case status of
        Active ->
            "Active"

        Completed ->
            "Completed"

        Important ->
            "Important"



-- PROGRAM
-------------------------------------------------------------------------------


main : Program () AppModel Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
