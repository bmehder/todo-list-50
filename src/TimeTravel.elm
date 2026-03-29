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
import Html.Events exposing (on, onCheck, onClick, onInput)
import Json.Decode as Decode



-- =========================================
-- TYPES
-- =========================================


type alias AppConfig msg model =
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    , msgToDebug : msg -> DebugInfo
    , modelToString : model -> String
    , decodeMsg : { index : Int, label : String, id : Maybe String } -> Maybe msg
    }


type alias Flags =
    { visibleByDefault : Bool
    }


type alias DebugInfo =
    { label : String
    , id : Maybe String
    }


type Msg msg
    = AppMsg msg
    | Prev
    | Next
    | ExportTimeline
    | ImportTextChanged String
    | ImportTimeline
    | ToggleVisibility Bool


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

        ExportTimeline ->
            applyExport msgToDebug (TimeTravel app)

        ImportTextChanged txt ->
            TimeTravel { app | importText = txt }

        ImportTimeline ->
            applyImport initModel updateModel decodeMsg app.importText (TimeTravel app)

        AppMsg msg ->
            applyAppMsg updateModel msg (TimeTravel app)

        ToggleVisibility isVisible ->
            TimeTravel { app | visibility = isVisible }


view :
    Config msg model
    -> TimeTravel msg model
    -> Html (Msg msg)
view config (TimeTravel app) =
    div [ class "flow" ]
        [ config.viewModel app.timeline.present
            |> Html.map AppMsg
        , div [ class "flex gap-1 align-items-center" ]
            [ input
                [ type_ "checkbox"
                , checked app.visibility
                , onCheck ToggleVisibility
                , id "toggle-debugger"
                ]
                []
            , Html.label
                [ Html.Attributes.for "toggle-debugger" ]
                [ text
                    (if app.visibility then
                        "Hide Time Travel Debugger"

                     else
                        "Show Time Travel Debugger"
                    )
                ]
            ]
        , if app.visibility then
            div [ class "flow" ]
                [ div [ class "flex gap-1" ]
                    [ button [ onClick Prev, disabled (List.isEmpty app.timeline.past) ] [ text "Prev" ]
                    , button [ onClick Next, disabled (List.isEmpty app.timeline.future) ] [ text "Next" ]
                    ]

                -- Timeline history (messages)
                , viewHistory config.msgToDebug config.modelToString app.timeline

                -- Tools section (Export / Import)
                , div [ class "flow" ]
                    [ Html.hr [ class "opacity-30" ] []
                    , div [ class "padding-top-2" ]
                        [ details [ class "flow" ]
                            [ summary []
                                [ text "📦 Tools: Export / Import" ]
                            , div [ class "flow" ]
                                [ button [ onClick ExportTimeline ] [ text "Export Timeline" ]
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
                                    , on "keydown" importKeyDecoder
                                    , placeholder "Paste timeline JSON here and press Enter to import"
                                    ]
                                    []
                                , button [ onClick ImportTimeline ] [ text "Import Timeline" ]
                                ]
                            ]
                        ]
                    ]
                ]

          else
            text ""
        ]


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
-- UPDATE ENGINE
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
                                , id = info.id
                                }
                            )

                initial =
                    { index = -1
                    , label = "InitialModel"
                    , id = Nothing
                    }
            in
            historyMessages ++ [ initial ]

        jsonString =
            let
                escape : String -> String
                escape str =
                    str
                        |> String.replace "\\" "\\\\"
                        |> String.replace "\"" "\\\""

                encodeItem item =
                    let
                        idPart =
                            case item.id of
                                Just id ->
                                    "\"id\": \"" ++ escape id ++ "\""

                                Nothing ->
                                    "\"id\": null"
                    in
                    "{"
                        ++ "\"index\": "
                        ++ String.fromInt item.index
                        ++ ", "
                        ++ "\"type\": \""
                        ++ escape item.label
                        ++ "\""
                        ++ ", "
                        ++ idPart
                        ++ "}"
            in
            "["
                ++ (messages
                        |> List.map encodeItem
                        |> String.join ", "
                   )
                ++ "]"
    in
    TimeTravel { app | exportText = Just jsonString }


applyImport :
    model
    -> (msg -> model -> model)
    -> ({ index : Int, label : String, id : Maybe String } -> Maybe msg)
    -> String
    -> TimeTravel msg model
    -> TimeTravel msg model
applyImport initModel updateModel decodeMsg importText (TimeTravel app) =
    let
        decoder =
            Decode.list
                (Decode.map3
                    (\i l id ->
                        { index = i
                        , label = l
                        , id = id
                        }
                    )
                    (Decode.field "index" Decode.int)
                    (Decode.field "type" Decode.string)
                    (Decode.field "id" (Decode.nullable Decode.string))
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
                }

        Err _ ->
            TimeTravel app



-- =========================================
-- VIEW HELPERS
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

        idText =
            case info.id of
                Just id ->
                    " " ++ id

                Nothing ->
                    ""

        summaryText =
            "Msg " ++ String.fromInt index ++ ": " ++ info.label ++ idText
    in
    details
        (name "frame"
            :: (if isOpen then
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
-- UTILITIES
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
            [ Html.div [ class "opacity-50 padding-top-1" ]
                (text "Before:"
                    :: List.map renderRemoved removedLines
                )
            ]
        ]



importKeyDecoder : Decode.Decoder (Msg msg)
importKeyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed ImportTimeline

                else
                    Decode.fail "ignore"
            )