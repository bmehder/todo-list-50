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
import Html exposing (Html, button, details, div, h2, summary, text)
import Html.Attributes exposing (class, disabled, id, name)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Html exposing (textarea)
import Html.Attributes exposing (placeholder)



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


view :
    Config msg model
    -> TimeTravel msg model
    -> Html (Msg msg)
view config (TimeTravel app) =
    div [ class "flow" ]
        [ config.viewModel app.timeline.present
            |> Html.map AppMsg
        , if app.visibility then
            div [ class "flow" ]
                [ h2 [] [ text "Time Travel Debugger" ]
                , div [ class "flex gap-1" ]
                    [ button [ onClick Prev, disabled (List.isEmpty app.timeline.past) ] [ text "Prev" ]
                    , button [ onClick Next, disabled (List.isEmpty app.timeline.future) ] [ text "Next" ]
                    ]
                , viewHistory config.msgToDebug config.modelToString app.timeline
                , details [class "flow"]
                    [ summary [ class "font-weight-bold" ] [ text "📦 Export / Import Timeline" ]
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
                            , placeholder "Paste timeline JSON here and click Import"
                            ]
                            []
                        , button [ onClick ImportTimeline ] [ text "Import Timeline" ]
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
                        viewFrame msgToDebug modelToString (total - 1 - i) frame
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
                [ name "frame" ]
                [ summary [ class "font-size-small" ] [ text "No Msg: Initial Model" ]
                , Html.pre [ class "font-size-small" ]
                    (String.lines (modelToString initialModelValue)
                        |> List.map (\line -> Html.div [] [ text line ])
                    )
                ]
    in
    div [ class "flow" ]
        (history
            ++ [ initial
               ]
        )


viewFrame : (msg -> DebugInfo) -> (model -> String) -> Int -> Frame msg model -> Html (Msg msg)
viewFrame msgToDebug modelToString index frame =
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
        [ name "frame" ]
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
        trimmed
            |> String.dropRight 1
            |> String.trimRight

    else
        trimmed


renderDiffLine : String -> String -> List (Html msg)
renderDiffLine before after =
    if normalizeLine before == normalizeLine after then
        [ Html.div [] [ text ("  " ++ after) ] ]

    else
        [ Html.div [ class "text-danger" ] [ text ("- " ++ before) ]
        , Html.div [ class "text-success" ] [ text ("+ " ++ after) ]
        ]


diffLines : String -> String -> List (Html msg)
diffLines before after =
    let
        beforeLines =
            String.lines before

        afterLines =
            String.lines after

        commonLength =
            Basics.min (List.length beforeLines) (List.length afterLines)

        paired =
            List.map2 Tuple.pair
                (List.take commonLength beforeLines)
                (List.take commonLength afterLines)

        diffs =
            paired
                |> List.concatMap (\( b, a ) -> renderDiffLine b a)

        extraBefore =
            List.drop commonLength beforeLines
                |> List.map (\line -> Html.div [ class "text-danger" ] [ text ("- " ++ line) ])

        extraAfter =
            List.drop commonLength afterLines
                |> List.map (\line -> Html.div [ class "text-success" ] [ text ("+ " ++ line) ])
    in
    diffs ++ extraBefore ++ extraAfter
