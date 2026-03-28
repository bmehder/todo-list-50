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
import Html.Events exposing (onClick)


type alias AppConfig msg model =
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    , msgToDebug : msg -> DebugInfo
    , modelToString : model -> String
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
        }


update :
    (msg -> model -> model)
    -> Msg msg
    -> TimeTravel msg model
    -> TimeTravel msg model
update updateModel timeTravelMsg (TimeTravel app) =
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
                ( update config.update msg model
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
    div [] (history ++ [ initial ])


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
