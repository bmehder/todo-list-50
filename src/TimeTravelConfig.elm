module TimeTravelConfig exposing
    ( modelToPrettyString
    , todoMsgToDebug
    )

import NonEmptyString
import NonNegative
import TimeTravel
import Types exposing (Editing(..), Filter(..), Id, Model, Status(..), Todo, Msg(..))



-- DEBUG HELPERS
-------------------------------------------------------------------------------


modelToPrettyString : Model -> String
modelToPrettyString model =
    "{\n"
        ++ "    draft = \""
        ++ model.draft
        ++ "\"\n"
        ++ "  , filter = "
        ++ filterToString model.filter
        ++ "\n"
        ++ "  , editing = "
        ++ editingToString model.editing
        ++ "\n"
        ++ "  , pendingDelete = "
        ++ pendingDeleteToString model.pendingDelete
        ++ "\n"
        ++ "  , todos = [\n"
        ++ (model.todos
                |> List.map todoToRecordString
                |> String.join ",\n"
           )
        ++ "\n    ]\n"
        ++ "}"


todoToRecordString : Todo -> String
todoToRecordString todo =
    "    { id = "
        ++ String.fromInt (NonNegative.toInt todo.id)
        ++ ", status = "
        ++ statusToString todo.status
        ++ ", important = "
        ++ (if todo.important then
                "True"

            else
                "False"
           )
        ++ ", todo text = \""
        ++ NonEmptyString.toString todo.todoText
        ++ "\" }"


statusToString : Status -> String
statusToString status =
    case status of
        Active ->
            "Active"

        Completed ->
            "Completed"


filterToString : Filter -> String
filterToString filter =
    case filter of
        All ->
            "All"

        ActiveOrImportantOnly ->
            "Active+Important"

        CompletedOnly ->
            "Completed"

        ImportantOnly ->
            "Important"


editingToString : Editing -> String
editingToString editing =
    case editing of
        NotEditing ->
            "NotEditing"

        EditingTodoText { id, draft } ->
            "EditingTodoText (id: "
                ++ String.fromInt (NonNegative.toInt id)
                ++ ", draft: \""
                ++ draft
                ++ "\")"


pendingDeleteToString : Maybe Id -> String
pendingDeleteToString maybeId =
    case maybeId of
        Nothing ->
            "Nothing"

        Just id ->
            "Just " ++ String.fromInt (NonNegative.toInt id)


todoMsgToDebug : Msg -> TimeTravel.DebugInfo
todoMsgToDebug msg =
    case msg of
        ToggledStatus id ->
            { label = "ToggledStatus"
            , id = Just (String.fromInt (NonNegative.toInt id))
            }

        ToggledImportant id ->
            { label = "ToggledImportant"
            , id = Just (String.fromInt (NonNegative.toInt id))
            }

        AskedToDelete id ->
            { label = "AskedToDelete"
            , id = Just (String.fromInt (NonNegative.toInt id))
            }

        ConfirmedDelete id ->
            { label = "ConfirmedDelete"
            , id = Just (String.fromInt (NonNegative.toInt id))
            }

        CanceledDelete ->
            { label = "CanceledDelete"
            , id = Nothing
            }

        UpdatedDraft str ->
            { label = "UpdatedDraft (typing) \"" ++ str ++ "\""
            , id = Nothing
            }

        SetFilter filter ->
            let
                filterLabel =
                    case filter of
                        All ->
                            "All"

                        ActiveOrImportantOnly ->
                            "ActiveAndImportantOnly"

                        CompletedOnly ->
                            "CompletedOnly"

                        ImportantOnly ->
                            "ImportantOnly"
            in
            { label = "SetFilter " ++ filterLabel
            , id = Nothing
            }

        CreatedTodo ->
            { label = "CreatedTodo"
            , id = Nothing
            }

        StartedEditingTodoText id draft ->
            { label =
                "StartedEditingTodoText \"" ++ draft ++ "\""
            , id = Just (String.fromInt (NonNegative.toInt id))
            }

        UpdatedEditingDraft str ->
            { label = "UpdatedEditingDraft (editing) \"" ++ str ++ "\""
            , id = Nothing
            }

        SavedEditedTodoText ->
            { label = "SavedEditedTodoText"
            , id = Nothing
            }

        CanceledEdit ->
            { label = "CanceledEdit"
            , id = Nothing
            }

        NoOp ->
            { label = "NoOp (ignored UI event)"
            , id = Nothing
            }
