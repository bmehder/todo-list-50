module TimeTravelConfig exposing
    ( decodeMsg
    , modelToPrettyString
    , todoMsgToDebug
    )

import NonEmptyString
import NonNegative
import TimeTravel
import Types exposing (Editing(..), Filter(..), Id, Model, Msg(..), Status(..), Todo)



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
        ++ (NonNegative.toInt todo.id |> String.fromInt)
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
                ++ (NonNegative.toInt id |> String.fromInt)
                ++ ", draft: \""
                ++ draft
                ++ "\")"


pendingDeleteToString : Maybe Id -> String
pendingDeleteToString maybeId =
    case maybeId of
        Nothing ->
            "Nothing"

        Just id ->
            "Just " ++ (NonNegative.toInt id |> String.fromInt)


todoMsgToDebug : Msg -> TimeTravel.DebugInfo
todoMsgToDebug msg =
    case msg of
        ToggledStatus id ->
            { label = "ToggledStatus"
            , id = Just (NonNegative.toInt id |> String.fromInt)
            }

        ToggledImportant id ->
            { label = "ToggledImportant"
            , id = Just (NonNegative.toInt id |> String.fromInt)
            }

        AskedToDelete id ->
            { label = "AskedToDelete"
            , id = Just (NonNegative.toInt id |> String.fromInt)
            }

        ConfirmedDelete id ->
            { label = "ConfirmedDelete"
            , id = Just (NonNegative.toInt id |> String.fromInt)
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
            , id = Just (NonNegative.toInt id |> String.fromInt)
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


matchPrefix : String -> (String -> Maybe msg) -> String -> Maybe msg
matchPrefix prefix toMsg str =
    if String.startsWith prefix str then
        str
            |> String.dropLeft (String.length prefix)
            |> toMsg

    else
        Nothing


stripQuotes : String -> String
stripQuotes str =
    str
        |> String.dropLeft 1
        |> String.dropRight 1


orElse : Maybe a -> Maybe a -> Maybe a
orElse first second =
    case first of
        Just _ ->
            first

        Nothing ->
            second


decodeMsg : { index : Int, label : String, id : Maybe String } -> Maybe Msg
decodeMsg item =
    let
        parseId =
            item.id
                |> Maybe.andThen String.toInt
    in
    case item.label of
        "NoOp (ignored UI event)" ->
            Just NoOp

        "SavedEditedTodoText" ->
            Just SavedEditedTodoText

        "CanceledEdit" ->
            Just CanceledEdit

        "ToggledStatus" ->
            parseId
                |> Maybe.andThen NonNegative.fromInt
                |> Maybe.map ToggledStatus

        "ToggledImportant" ->
            parseId
                |> Maybe.andThen NonNegative.fromInt
                |> Maybe.map ToggledImportant

        "AskedToDelete" ->
            parseId
                |> Maybe.andThen NonNegative.fromInt
                |> Maybe.map AskedToDelete

        "ConfirmedDelete" ->
            parseId
                |> Maybe.andThen NonNegative.fromInt
                |> Maybe.map ConfirmedDelete

        "CanceledDelete" ->
            Just CanceledDelete

        "SetFilter ActiveAndImportantOnly" ->
            Just (SetFilter ActiveOrImportantOnly)

        "SetFilter CompletedOnly" ->
            Just (SetFilter CompletedOnly)

        "SetFilter All" ->
            Just (SetFilter All)

        "SetFilter ImportantOnly" ->
            Just (SetFilter ImportantOnly)

        "CreatedTodo" ->
            Just CreatedTodo

        labelStr ->
            matchPrefix "UpdatedDraft (typing) "
                (stripQuotes
                    >> UpdatedDraft
                    >> Just
                )
                labelStr
                |> orElse
                    (matchPrefix "UpdatedEditingDraft (editing) "
                        (stripQuotes
                            >> UpdatedEditingDraft
                            >> Just
                        )
                        labelStr
                    )
                |> orElse
                    (if String.startsWith "StartedEditingTodoText " labelStr then
                        parseId
                            |> Maybe.andThen NonNegative.fromInt
                            |> Maybe.map (\idVal -> StartedEditingTodoText idVal "")

                     else
                        Nothing
                    )
