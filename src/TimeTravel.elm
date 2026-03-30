module TimeTravel exposing
    ( DebugInfo
    , Flags
    , Frame
    , Msg(..)
    , TimeTravel
    , Timeline
    , init
    , update
    , view
    , withTimeTravel
    )

import Browser
import Html exposing (Html, button, details, div, input, summary, text, textarea)
import Html.Attributes exposing (attribute, checked, class, disabled, id, name, placeholder, type_)
import Html.Events exposing (custom, onCheck, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode



-- =========================================
-- TYPES
-- =========================================


type alias AppConfig msg model =
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    , msgToDebug : msg -> DebugInfo
    , modelToString : model -> String
    , decodeMsg :
        { index : Int
        , label : String
        , payload : Maybe Decode.Value
        }
        -> Maybe msg
    }


type alias Flags =
    { visibleByDefault : Bool
    }


type alias DebugInfo =
    { label : String
    , payload : Maybe Encode.Value
    }


type Msg msg
    = AppMsg msg
    | Prev
    | Next
    | ExportedTimeline
    | ImportTextChanged String
    | ImportedTimeline
    | ToggledVisibility Bool


type alias Frame msg model =
    { msg : msg
    , prev : model
    , next : model
    }


type alias Timeline msg model =
    { past : List (Frame msg model)
    , present : model
    , future : List (Frame msg model)
    }


type TimeTravel msg model
    = TimeTravel
        { timeline : Timeline msg model
        , visibility : Bool
        , exportText : Maybe String
        , importText : String
        , importStatus : Maybe String
        }


type alias Config msg model =
    { viewModel : model -> Html msg
    , msgToDebug : msg -> DebugInfo
    , modelToString : model -> String
    }



-- =========================================
-- PUBLIC API
-- =========================================


init : Bool -> model -> TimeTravel msg model
init visible model =
    TimeTravel
        { timeline =
            { past = []
            , present = model
            , future = []
            }
        , visibility = visible
        , exportText = Nothing
        , importText = ""
        , importStatus = Nothing
        }


update :
    AppConfig msg model
    -> Msg msg
    -> TimeTravel msg model
    -> TimeTravel msg model
update config timeTravelMsg (TimeTravel app) =
    let
        initModel =
            config.init

        updateModel =
            config.update

        msgToDebug =
            config.msgToDebug

        decodeMsg =
            config.decodeMsg
    in
    case timeTravelMsg of
        Prev ->
            applyPrev (TimeTravel app)

        Next ->
            applyNext (TimeTravel app)

        ExportedTimeline ->
            applyExport msgToDebug (TimeTravel app)

        ImportTextChanged txt ->
            TimeTravel { app | importText = txt, importStatus = Nothing }

        ImportedTimeline ->
            applyImport initModel updateModel decodeMsg app.importText (TimeTravel app)

        AppMsg msg ->
            applyAppMsg updateModel msg (TimeTravel app)

        ToggledVisibility isVisible ->
            TimeTravel { app | visibility = isVisible }


view :
    Config msg model
    -> TimeTravel msg model
    -> Html (Msg msg)
view config (TimeTravel app) =
    div [ class "flow" ]
        [ config.viewModel app.timeline.present
            |> Html.map AppMsg
        , viewToggle app
        , if app.visibility then
            viewDebugger config app

          else
            text ""
        ]


viewToggle : { a | visibility : Bool } -> Html (Msg msg)
viewToggle app =
    div [ class "flex gap-1 align-items-center" ]
        [ input
            [ type_ "checkbox"
            , checked app.visibility
            , onCheck ToggledVisibility
            , id "toggle-debugger"
            ]
            []
        , Html.label
            [ Html.Attributes.for "toggle-debugger" ]
            [ text "Show Time Travel Debugger"
            ]
        ]


viewNav : Timeline msg model -> Html (Msg msg)
viewNav timeline =
    div [ class "flex gap-1" ]
        [ button [ onClick Prev, disabled (List.isEmpty timeline.past) ] [ text "Prev" ]
        , button [ onClick Next, disabled (List.isEmpty timeline.future) ] [ text "Next" ]
        ]


viewTools : { a | exportText : Maybe String, importText : String, importStatus : Maybe String } -> Html (Msg msg)
viewTools app =
    div [ class "flow" ]
        [ Html.hr [ class "opacity-30" ] []
        , div [ ]
            [ details [ class "flow" ]
                [ summary []
                    [ text "📦 Tools: Export / Import" ]
                , div [ class "flow" ]
                    [ button [ onClick ExportedTimeline ] [ text "Export Timeline" ]
                    , textarea
                        [ class "width-100 min-height-10"
                        , id "export"
                        , placeholder "Click 'Export Timeline' to generate JSON"
                        ]
                        [ text (Maybe.withDefault "" app.exportText) ]
                    , textarea
                        [ class "width-100 min-height-10"
                        , id "import"
                        , onInput ImportTextChanged
                        , custom "keydown" importKeyDecoder
                        , Html.Attributes.value app.importText
                        , placeholder "Paste timeline JSON here and press Enter to import"
                        ]
                        []
                    , button [ onClick ImportedTimeline ] [ text "Import Timeline" ]
                    , case app.importStatus of
                        Just msg ->
                            div [ class "font-size-small opacity-70 padding-block-1" ] [ text msg ]

                        Nothing ->
                            text ""
                    ]
                ]
            ]
        ]


viewDebugger : Config msg model -> { a | timeline : Timeline msg model, exportText : Maybe String, importText : String, importStatus : Maybe String } -> Html (Msg msg)
viewDebugger config app =
    div [ class "flow" ]
        [ viewNav app.timeline
        , viewHistory config.msgToDebug config.modelToString app.timeline
        , viewTools app
        ]



-- =========================================
-- PROGRAM WIRING
-- =========================================


withTimeTravel :
    AppConfig msg model
    -> Program Flags (TimeTravel msg model) (Msg msg)
withTimeTravel config =
    Browser.element
        { init =
            \flags ->
                ( init flags.visibleByDefault config.init
                , Cmd.none
                )
        , update =
            \msg model ->
                ( update config msg model
                , Cmd.none
                )
        , view =
            \model ->
                view
                    { viewModel = config.view
                    , msgToDebug = config.msgToDebug
                    , modelToString = config.modelToString
                    }
                    model
        , subscriptions = \_ -> Sub.none
        }



-- =========================================
-- UPDATE ENGINE (CORE STATE LOGIC)
-- =========================================


rebuildTimeline :
    model
    -> (msg -> model -> model)
    -> List msg
    -> ( model, List (Frame msg model) )
rebuildTimeline initModel updateModel msgs =
    List.foldl
        (\msg ( prevModel, frames ) ->
            let
                nextModel =
                    updateModel msg prevModel

                frame =
                    { msg = msg
                    , prev = prevModel
                    , next = nextModel
                    }
            in
            ( nextModel, frame :: frames )
        )
        ( initModel, [] )
        msgs


applyPrev : TimeTravel msg model -> TimeTravel msg model
applyPrev (TimeTravel app) =
    case app.timeline.past of
        [] ->
            TimeTravel app

        frame :: rest ->
            TimeTravel
                { app
                    | timeline =
                        { past = rest
                        , present = frame.prev
                        , future = frame :: app.timeline.future
                        }
                }


applyNext : TimeTravel msg model -> TimeTravel msg model
applyNext (TimeTravel app) =
    case app.timeline.future of
        [] ->
            TimeTravel app

        frame :: rest ->
            TimeTravel
                { app
                    | timeline =
                        { past = frame :: app.timeline.past
                        , present = frame.next
                        , future = rest
                        }
                }


applyAppMsg :
    (msg -> model -> model)
    -> msg
    -> TimeTravel msg model
    -> TimeTravel msg model
applyAppMsg updateModel msg (TimeTravel app) =
    let
        timeline =
            app.timeline

        newModel =
            updateModel msg timeline.present

        frame =
            { msg = msg
            , prev = timeline.present
            , next = newModel
            }

        newTimeline =
            { past = frame :: timeline.past
            , present = newModel
            , future = []
            }
    in
    TimeTravel { app | timeline = newTimeline }



-- =========================================
-- EXPORT / IMPORT (SERIALIZATION)
-- =========================================


applyExport :
    (msg -> DebugInfo)
    -> TimeTravel msg model
    -> TimeTravel msg model
applyExport msgToDebug (TimeTravel app) =
    let
        timeline =
            app.timeline

        messages =
            let
                historyMessages =
                    timeline.past
                        |> List.reverse
                        |> List.indexedMap
                            (\i frame ->
                                let
                                    info =
                                        msgToDebug frame.msg
                                in
                                { index = i
                                , label = info.label
                                , payload = info.payload
                                }
                            )

                initial =
                    { index = -1
                    , label = "InitialModel"
                    , payload = Nothing
                    }
            in
            historyMessages ++ [ initial ]

        encodeItem item =
            let
                baseFields =
                    [ ( "index", Encode.int item.index )
                    , ( "type", Encode.string item.label )
                    ]

                payloadField =
                    case item.payload of
                        Just payload ->
                            [ ( "payload", payload ) ]

                        Nothing ->
                            []
            in
            Encode.object (baseFields ++ payloadField)

        jsonString =
            Encode.encode 4 (Encode.list encodeItem messages)
    in
    TimeTravel { app | exportText = Just jsonString }


applyImport :
    model
    -> (msg -> model -> model)
    -> ({ index : Int, label : String, payload : Maybe Decode.Value } -> Maybe msg)
    -> String
    -> TimeTravel msg model
    -> TimeTravel msg model
applyImport initModel updateModel decodeMsg importText (TimeTravel app) =
    let
        decoder =
            Decode.list
                (Decode.map3
                    (\i l payload ->
                        { index = i
                        , label = l
                        , payload = payload
                        }
                    )
                    (Decode.field "index" Decode.int)
                    (Decode.field "type" Decode.string)
                    (Decode.oneOf
                        [ Decode.field "payload" Decode.value |> Decode.map Just
                        , Decode.succeed Nothing
                        ]
                    )
                )
    in
    case Decode.decodeString decoder importText of
        Ok items ->
            let
                decodedMsgs =
                    items
                        |> List.filter (\item -> item.index >= 0)
                        |> List.sortBy .index
                        |> List.filterMap decodeMsg

                ( finalModel, frames ) =
                    rebuildTimeline initModel updateModel decodedMsgs
            in
            TimeTravel
                { app
                    | timeline =
                        { past = frames
                        , present = finalModel
                        , future = []
                        }
                    , importStatus = Just "Imported ✔"
                }

        Err _ ->
            TimeTravel { app | importStatus = Just "Import failed" }



-- =========================================
-- VIEW (DEBUGGER UI)
-- =========================================


viewHistory : (msg -> DebugInfo) -> (model -> String) -> Timeline msg model -> Html (Msg msg)
viewHistory msgToDebug modelToString timeline =
    let
        total =
            List.length timeline.past

        history =
            timeline.past
                |> List.indexedMap
                    (\i frame ->
                        viewFrame msgToDebug modelToString (total - 1 - i) (i == 0) frame
                    )

        initial =
            let
                initialModelValue =
                    case List.reverse timeline.past of
                        [] ->
                            timeline.present

                        frame :: _ ->
                            frame.prev
            in
            details
                (name "frame"
                    :: (if List.isEmpty timeline.past then
                            [ attribute "open" "" ]

                        else
                            []
                       )
                )
                [ summary [ class "font-size-small" ] [ text "No Msg: Initial Model" ]
                , Html.pre [ class "font-size-small" ]
                    (String.lines (modelToString initialModelValue)
                        |> List.map (\line -> Html.div [] [ text line ])
                    )
                ]
    in
    div [ class "flow" ]
        (history ++ [ initial ])


viewFrame : (msg -> DebugInfo) -> (model -> String) -> Int -> Bool -> Frame msg model -> Html (Msg msg)
viewFrame msgToDebug modelToString index isOpen frame =
    let
        info =
            msgToDebug frame.msg

        summaryText =
            "Msg " ++ String.fromInt index ++ ": " ++ info.label
    in
    details
        ([ class "padding-block-end-1 border-bottom-1"
         , name "frame"
         ]
            ++ (if isOpen then
                    [ attribute "open" "" ]

                else
                    []
               )
        )
        [ summary [ class "font-size-small" ] [ text summaryText ]
        , Html.pre [ class "font-size-small" ]
            (diffLines
                (modelToString frame.prev)
                (modelToString frame.next)
            )
        ]



-- =========================================
-- UTILITIES (INTERNAL HELPERS)
-- =========================================


normalizeLine : String -> String
normalizeLine line =
    let
        trimmed =
            String.trim line
    in
    if String.endsWith "," trimmed then
        String.dropRight 1 trimmed

    else
        trimmed


diffLines : String -> String -> List (Html msg)
diffLines before after =
    let
        beforeLines =
            String.lines before

        afterLines =
            String.lines after

        hasMatchingLine lines target =
            let
                normalizedTarget =
                    normalizeLine target
            in
            List.any (\line -> normalizeLine line == normalizedTarget) lines

        removedLines =
            beforeLines
                |> List.filter (\line -> not (hasMatchingLine afterLines line))

        renderAfterLine line =
            if hasMatchingLine beforeLines line then
                Html.div [] [ text ("  " ++ line) ]

            else
                Html.div [ class "text-success" ] [ text ("+ " ++ line) ]

        renderRemoved line =
            Html.div [ class "text-danger" ] [ text ("- " ++ line) ]
    in
    List.concat
        [ afterLines |> List.map renderAfterLine
        , if List.isEmpty removedLines then
            []

          else
            [ Html.div [ class "opacity-50 padding-block-1" ]
                (text "Before:"
                    :: List.map renderRemoved removedLines
                )
            ]
        ]


importKeyDecoder : Decode.Decoder { message : Msg msg, stopPropagation : Bool, preventDefault : Bool }
importKeyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed
                        { message = ImportedTimeline
                        , stopPropagation = False
                        , preventDefault = True
                        }

                else
                    Decode.fail "ignore"
            )
