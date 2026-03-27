module TimeTravel exposing
    ( DebugInfo
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
import Html exposing (Html, button, details, div, input, label, summary, text)
import Html.Attributes exposing (checked, class, disabled, for, id, name, type_)
import Html.Events exposing (onClick)


type alias AppConfig msg model =
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    , msgToDebug : msg -> DebugInfo
    , modelToString : model -> String
    , visibleByDefault : Bool
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
    -> Program () (TimeTravel msg model) (Msg msg)
withTimeTravel config =
    Browser.sandbox
        { init = init config.visibleByDefault config.init
        , update = update config.update
        , view =
            \model ->
                view
                    { viewModel = config.view
                    , msgToDebug = config.msgToDebug
                    , modelToString = config.modelToString
                    }
                    model
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
                [ div [ class "flex gap-1" ]
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
            details
                [ name "frame" ]
                [ summary [ class "font-size-small" ] [ text "No Msg: Initial Model" ]
                , Html.pre [ class "font-size-small" ] [ text (modelToString (initialModel timeline)) ]
                ]
    in
    div [] (history ++ [ initial ])


initialModel : Timeline msg model -> model
initialModel timeline =
    case List.reverse timeline.past of
        [] ->
            timeline.present

        frame :: _ ->
            frame.prev


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
            [ text (modelToString frame.next) ]
        ]
