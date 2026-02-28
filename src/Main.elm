module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, input, li, span, text, ul)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode



-- MODEL


type alias Id =
    Int


type Status
    = Active
    | Completed


type alias Todo =
    { id : Id
    , item : String
    , status : Status
    }


type alias Model =
    { todos : List Todo
    , nextId : Id
    , draft : String
    }


init : Model
init =
    { todos =
        [ { id = 0, item = "Buy coffee", status = Active }
        , { id = 1, item = "Write the smallest Elm app", status = Completed }
        , { id = 2, item = "Profit", status = Active }
        ]
    , nextId = 3
    , draft = ""
    }



-- UPDATE


type Msg
    = ToggleTodo Id
    | DeleteTodo Id
    | UpdateDraft String
    | CreateTodo


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleTodo id ->
            { model
                | todos = toggleById id <| model.todos
            }

        DeleteTodo id ->
            { model
                | todos = deleteById id <| model.todos
            }

        UpdateDraft newValue ->
            { model | draft = newValue }

        CreateTodo ->
            createIfValid model



-- DOMAIN HELPERS


toggle : Todo -> Todo
toggle todo =
    { todo
        | status =
            case todo.status of
                Active ->
                    Completed

                Completed ->
                    Active
    }


toggleIfMatch : Id -> Todo -> Todo
toggleIfMatch id todo =
    if todo.id == id then
        toggle todo

    else
        todo


toggleById : Id -> List Todo -> List Todo
toggleById id =
    List.map (toggleIfMatch id)


matchesId : Id -> Todo -> Bool
matchesId id todo =
    todo.id == id


deleteById : Id -> List Todo -> List Todo
deleteById id =
    List.filter (not << matchesId id)


createIfValid : Model -> Model
createIfValid model =
    if String.trim model.draft == "" then
        model

    else
        let
            newTodo =
                { id = model.nextId
                , item = model.draft
                , status = Active
                }
        in
        { model
            | todos = model.todos ++ [ newTodo ]
            , nextId = model.nextId + 1
            , draft = ""
        }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flow" ]
        [ h1 [] [ text "Todo List" ]
        , viewCreate model
        , ul [ class "flow" ] (List.map viewTodo model.todos)
        ]


viewCreate : Model -> Html Msg
viewCreate model =
    form
        [ class "grid gap-1"
        , onSubmit CreateTodo
        ]
        [ input
            [ value model.draft
            , onInput UpdateDraft
            , placeholder "Add a todo..."
            ]
            []
        , button
            [ type_ "submit" ]
            [ text "Add" ]
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    li [ class "flex space-between align-items-center gap-1", onClick (ToggleTodo todo.id) ]
        [ viewItem todo
        , viewDeleteButton todo
        ]


viewItem : Todo -> Html Msg
viewItem todo =
    case todo.status of
        Active ->
            span
                [ class "cursor-pointer" ]
                [ text todo.item ]

        Completed ->
            span
                [ class "line-through opacity-60 cursor-pointer" ]
                [ text todo.item ]


viewDeleteButton : Todo -> Html Msg
viewDeleteButton todo =
    button
        [ stopPropagationOn "click" (Decode.succeed ( DeleteTodo todo.id, True ))
        , class "delete-btn cursor-pointer"
        ]
        [ text "✕" ]



-- PROGRAM


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
