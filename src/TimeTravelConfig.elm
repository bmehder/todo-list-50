module TimeTravelConfig exposing
    ( decodeMsg
    , modelToPrettyString
    , msgToDebugInfo
    )

import NonEmptyString
import NonNegative
import TimeTravel
import Types exposing (Editing(..), Filter(..), Id, Model, Msg(..), Status(..), Todo)



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


decodeMsg : { index : Int, label : String, id : Maybe String } -> Maybe Msg
decodeMsg item =
    decodeExact item
        |> orElse (decodeWithId item)
        |> orElse (decodePattern item)



-- DECODE HELPERS
-------------------------------------------------------------------------------


decodeExact : { index : Int, label : String, id : Maybe String } -> Maybe Msg
decodeExact item =
    case item.label of
        "NoOp (ignored UI event)" ->
            Just NoOp

        "SavedEditedTodoText" ->
            Just SavedEditedTodoText

        "CanceledEdit" ->
            Just CanceledEdit

        "CanceledDelete" ->
            Just CanceledDelete

        "CreatedTodo" ->
            Just CreatedTodo

        "SetFilter ActiveAndImportantOnly" ->
            Just (SetFilter ActiveOrImportantOnly)

        "SetFilter CompletedOnly" ->
            Just (SetFilter CompletedOnly)

        "SetFilter All" ->
            Just (SetFilter All)

        "SetFilter ImportantOnly" ->
            Just (SetFilter ImportantOnly)

        _ ->
            Nothing


decodeWithId : { index : Int, label : String, id : Maybe String } -> Maybe Msg
decodeWithId item =
    case item.label of
        "ToggledStatus" ->
            withId item.id ToggledStatus

        "ToggledImportant" ->
            withId item.id ToggledImportant

        "AskedToDelete" ->
            withId item.id AskedToDelete

        "ConfirmedDelete" ->
            withId item.id ConfirmedDelete

        _ ->
            Nothing


decodePattern : { index : Int, label : String, id : Maybe String } -> Maybe Msg
decodePattern item =
    let
        labelStr =
            item.label
    in
    match "UpdatedDraft (typing) " UpdatedDraft labelStr
        |> orElse (match "UpdatedEditingDraft (editing) " UpdatedEditingDraft labelStr)
        |> orElse (decodeStartedEditing item)


decodeStartedEditing : { index : Int, label : String, id : Maybe String } -> Maybe Msg
decodeStartedEditing item =
    if String.startsWith "StartedEditingTodoText " item.label then
        let
            draft =
                item.label
                    |> String.dropLeft (String.length "StartedEditingTodoText ")
                    |> stripQuotes
        in
        withId item.id (\idVal -> StartedEditingTodoText idVal draft)

    else
        Nothing


withId : Maybe String -> (NonNegative.NonNegative -> Msg) -> Maybe Msg
withId maybeId toMsg =
    maybeId
        |> Maybe.andThen String.toInt
        |> Maybe.andThen NonNegative.fromInt
        |> Maybe.map toMsg


orElse : Maybe a -> Maybe a -> Maybe a
orElse first second =
    case first of
        Just _ ->
            first

        Nothing ->
            second


match : String -> (String -> Msg) -> String -> Maybe Msg
match prefix toMsg str =
    matchPrefix prefix (stripQuotes >> toMsg >> Just) str



-- INTERNAL HELPERS
-------------------------------------------------------------------------------


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
