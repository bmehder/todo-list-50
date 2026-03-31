module TimeTravelConfig exposing
    ( decodeMsg
    , modelToPrettyString
    , msgToDebugInfo
    )

import Json.Decode as Decode
import Json.Encode as Encode
import NonEmptyString
import NonNegativeInt
import TimeTravel
import Types exposing (Editing(..), Filter(..), Id, Model, Msg(..), Status(..), Todo)


type alias TimelineItem =
    { index : Int
    , label : String
    , payload : Maybe Decode.Value
    }



-- PUBLIC API
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


msgToDebugInfo : Msg -> TimeTravel.DebugInfo
msgToDebugInfo msg =
    case msg of
        ToggledStatus id ->
            { label = "ToggledStatus"
            , payload =
                Just
                    (Encode.object
                        [ ( "id", Encode.int (NonNegativeInt.toInt id) )
                        ]
                    )
            }

        ToggledImportant id ->
            { label = "ToggledImportant"
            , payload =
                Just
                    (Encode.object
                        [ ( "id", Encode.int (NonNegativeInt.toInt id) )
                        ]
                    )
            }

        AskedToDelete id ->
            { label = "AskedToDelete"
            , payload =
                Just
                    (Encode.object
                        [ ( "id", Encode.int (NonNegativeInt.toInt id) )
                        ]
                    )
            }

        ConfirmedDelete id ->
            { label = "ConfirmedDelete"
            , payload =
                Just
                    (Encode.object
                        [ ( "id", Encode.int (NonNegativeInt.toInt id) )
                        ]
                    )
            }

        CanceledDelete ->
            { label = "CanceledDelete"
            , payload = Nothing
            }

        UpdatedDraft str ->
            { label = "UpdatedDraft"
            , payload =
                Just
                    (Encode.object
                        [ ( "value", Encode.string str )
                        ]
                    )
            }

        SetFilter filter ->
            { label = "SetFilter"
            , payload =
                Just
                    (Encode.object
                        [ ( "filter", Encode.string (filterTag filter) )
                        ]
                    )
            }

        CreatedTodo ->
            { label = "CreatedTodo"
            , payload = Nothing
            }

        StartedEditingTodoText id draft ->
            { label = "StartedEditingTodoText"
            , payload =
                Just
                    (Encode.object
                        [ ( "id", Encode.int (NonNegativeInt.toInt id) )
                        , ( "draft", Encode.string draft )
                        ]
                    )
            }

        UpdatedEditingDraft str ->
            { label = "UpdatedEditingDraft"
            , payload =
                Just
                    (Encode.object
                        [ ( "value", Encode.string str )
                        ]
                    )
            }

        SavedEditedTodoText ->
            { label = "SavedEditedTodoText"
            , payload = Nothing
            }

        CanceledEdit ->
            { label = "CanceledEdit"
            , payload = Nothing
            }

        NoOp ->
            { label = "NoOp"
            , payload = Nothing
            }


decodeMsg : TimelineItem -> Maybe Msg
decodeMsg item =
    case item.label of
        "ToggledStatus" ->
            decodeIdPayload ToggledStatus item.payload

        "ToggledImportant" ->
            decodeIdPayload ToggledImportant item.payload

        "AskedToDelete" ->
            decodeIdPayload AskedToDelete item.payload

        "ConfirmedDelete" ->
            decodeIdPayload ConfirmedDelete item.payload

        "CanceledDelete" ->
            Just CanceledDelete

        "UpdatedDraft" ->
            decodeStringValuePayload UpdatedDraft item.payload

        "SetFilter" ->
            decodeFilterPayload item.payload

        "CreatedTodo" ->
            Just CreatedTodo

        "StartedEditingTodoText" ->
            decodeStartedEditingPayload item.payload

        "UpdatedEditingDraft" ->
            decodeStringValuePayload UpdatedEditingDraft item.payload

        "SavedEditedTodoText" ->
            Just SavedEditedTodoText

        "CanceledEdit" ->
            Just CanceledEdit

        "NoOp" ->
            Just NoOp

        _ ->
            Nothing



-- DECODE HELPERS
-------------------------------------------------------------------------------


decodeIdPayload : (Id -> Msg) -> Maybe Decode.Value -> Maybe Msg
decodeIdPayload toMsg maybePayload =
    maybePayload
        |> Maybe.andThen decodeIdValue
        |> Maybe.map toMsg


decodeIdValue : Decode.Value -> Maybe Id
decodeIdValue =
    Decode.decodeValue (Decode.field "id" Decode.int)
        >> Result.toMaybe
        >> Maybe.andThen NonNegativeInt.fromInt


decodeStringValuePayload : (String -> Msg) -> Maybe Decode.Value -> Maybe Msg
decodeStringValuePayload toMsg maybePayload =
    maybePayload
        |> Maybe.andThen
            (Decode.decodeValue (Decode.field "value" Decode.string)
                >> Result.toMaybe
            )
        |> Maybe.map toMsg


decodeFilterPayload : Maybe Decode.Value -> Maybe Msg
decodeFilterPayload maybePayload =
    maybePayload
        |> Maybe.andThen
            (Decode.decodeValue (Decode.field "filter" Decode.string)
                >> Result.toMaybe
                >> Maybe.andThen filterFromTag
                >> Maybe.map SetFilter
            )


decodeStartedEditingPayload : Maybe Decode.Value -> Maybe Msg
decodeStartedEditingPayload maybePayload =
    maybePayload
        |> Maybe.andThen
            (Decode.decodeValue startedEditingDecoder
                >> Result.toMaybe
                >> Maybe.map
                    (\{ id, draft } ->
                        StartedEditingTodoText id draft
                    )
            )


startedEditingDecoder : Decode.Decoder { id : Id, draft : String }
startedEditingDecoder =
    Decode.field "id" Decode.int
        |> Decode.andThen
            (\id ->
                case NonNegativeInt.fromInt id of
                    Just id_ ->
                        Decode.map
                            (\draft ->
                                { id = id_
                                , draft = draft
                                }
                            )
                            (Decode.field "draft" Decode.string)

                    Nothing ->
                        Decode.fail "Invalid non-negative id"
            )



-- INTERNAL HELPERS
-------------------------------------------------------------------------------


idToString : Id -> String
idToString =
    NonNegativeInt.toInt >> String.fromInt


filterTag : Filter -> String
filterTag filter =
    case filter of
        All ->
            "All"

        ActiveOrImportantOnly ->
            "ActiveOrImportantOnly"

        CompletedOnly ->
            "CompletedOnly"

        ImportantOnly ->
            "ImportantOnly"


filterFromTag : String -> Maybe Filter
filterFromTag filterStr =
    case filterStr of
        "All" ->
            Just All

        "ActiveOrImportantOnly" ->
            Just ActiveOrImportantOnly

        "CompletedOnly" ->
            Just CompletedOnly

        "ImportantOnly" ->
            Just ImportantOnly

        _ ->
            Nothing


todoToRecordString : Todo -> String
todoToRecordString todo =
    "    { id = "
        ++ idToString todo.id
        ++ ", status = "
        ++ statusToString todo.status
        ++ ", important = "
        ++ (if todo.important then
                "True"

            else
                "False"
           )
        ++ ", todoText = \""
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
                ++ idToString id
                ++ ", draft: \""
                ++ draft
                ++ "\")"


pendingDeleteToString : Maybe Id -> String
pendingDeleteToString maybeId =
    case maybeId of
        Nothing ->
            "Nothing"

        Just id ->
            "Just " ++ idToString id
