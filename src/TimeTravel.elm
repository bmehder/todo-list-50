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
import Json.Decode as Decode exposing (Decoder)


type alias AppConfig msg model =
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    , msgToDebug : msg -> DebugInfo
    , modelToString : model -> String
    , decodeMsg : { index : Int, type_ : String, id : Maybe String } -> Maybe msg
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
    model
    -> (msg -> model -> model)
    -> (msg -> DebugInfo)
    -> ({ index : Int, type_ : String, id : Maybe String } -> Maybe msg)
    -> Msg msg
    -> TimeTravel msg model
    -> TimeTravel msg model
update initModel updateModel msgToDebug decodeMsg timeTravelMsg (TimeTravel app) =
    case timeTravelMsg of
        Prev ->
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

        Next ->
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

        ExportTimeline ->
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
                                        , type_ = info.label
                                        , id = info.id
                                        }
                                    )

                        initial =
                            { index = -1
                            , type_ = "InitialModel"
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
                                ++ "\"index\": " ++ String.fromInt item.index ++ ", "
                                ++ "\"type\": \"" ++ escape item.type_ ++ "\", "
                                ++ idPart
                                ++ "}"
                    in
                    "["
                        ++ (messages
                                |> List.map encodeItem
                                |> String.join ", "
                           )
                        ++ "]"

                _ =
                    Debug.log "Export Timeline (JSON)" jsonString
            in
            TimeTravel { app | exportText = Just jsonString }

        ImportTextChanged txt ->
            TimeTravel { app | importText = txt }

        ImportTimeline ->
            let
                decoder : Decoder (List { index : Int, type_ : String, id : Maybe String })
                decoder =
                    Decode.list
                        (Decode.map3
                            (\i l id ->
                                { index = i
                                , type_ = l
                                , id = id
                                }
                            )
                            (Decode.field "index" Decode.int)
                            (Decode.field "type" Decode.string)
                            (Decode.field "id" (Decode.nullable Decode.string))
                        )

                result =
                    Decode.decodeString decoder app.importText
            in
            case result of
                Ok items ->
                    let
                        _ =
                            Debug.log "Import OK (count)" (List.length items)

                        _ =
                            Debug.log "Decoded messages"
                                (items |> List.map decodeMsg)

                        framesRebuilt =
                            let
                                msgs =
                                    items
                                        |> List.filter (\item -> item.index >= 0)
                                        |> List.sortBy .index
                                        |> List.filterMap decodeMsg

                                _ =
                                    Debug.log "Replay msgs" msgs
                            in
                            msgs
                                |> List.foldl
                                    (\msg (prevModel, frames) ->
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

                        newTimeline =
                            case framesRebuilt of
                                ( finalModel, frames ) ->
                                    { past = frames
                                    , present = finalModel
                                    , future = []
                                    }
                    in
                    TimeTravel { app | timeline = newTimeline }

                Err err ->
                    let
                        _ =
                            Debug.log "Import error" err
                    in
                    TimeTravel app

        AppMsg msg ->
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
                ( update config.init config.update config.msgToDebug config.decodeMsg msg model
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
                , case app.exportText of
                    Just txt ->
                        Html.textarea [ class "width-100 min-height-10", id "export" ] [ text txt ]

                    Nothing ->
                        text ""
                , Html.textarea
                    [ class "width-100 min-height-10"
                    , id "import"
                    , onInput ImportTextChanged
                    ]
                    []
                , button [ onClick ImportTimeline ] [ text "Import Timeline" ]
                ]
          else
            text ""
        ]


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
    div []
        (history
            ++ [ initial
               , button [ onClick ExportTimeline ] [ text "Export Timeline" ]
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
                |> List.concatMap
                    (\( b, a ) ->
                        let
                            normalize line =
                                line
                                    |> String.trim
                                    |> (\l ->
                                            if String.endsWith "," l then
                                                String.dropRight 1 l |> String.trimRight

                                            else
                                                l
                                       )
                        in
                        if normalize b == normalize a then
                            [ Html.div [] [ text ("  " ++ a) ] ]

                        else
                            [ Html.div [ class "text-danger" ] [ text ("- " ++ b) ]
                            , Html.div [ class "text-success" ] [ text ("+ " ++ a) ]
                            ]
                    )

        extraBefore =
            List.drop commonLength beforeLines
                |> List.map (\line -> Html.div [ class "text-danger" ] [ text ("- " ++ line) ])

        extraAfter =
            List.drop commonLength afterLines
                |> List.map (\line -> Html.div [ class "text-success" ] [ text ("+ " ++ line) ])
    in
    diffs ++ extraBefore ++ extraAfter
