module Types exposing (..)

import NonEmptyString exposing (NonEmptyString)
import NonNegativeInt exposing (NonNegativeInt)



-- =========================================
-- MODEL
-- =========================================


type alias Model =
    { todos : List Todo
    , draft : String
    , filter : Filter
    , editing : Editing
    , pendingDelete : Maybe Id
    }


type alias Todo =
    { id : Id
    , todoText : TodoText
    , status : Status
    , important : Bool
    }


type alias TodoText =
    NonEmptyString


type alias Id =
    NonNegativeInt



-- =========================================
-- DOMAIN TYPES
-- =========================================


type Status
    = Active
    | Completed


type Filter
    = All
    | ActiveOrImportantOnly
    | CompletedOnly
    | ImportantOnly


type Editing
    = NotEditing
    | EditingTodoText
        { id : Id
        , draft : String
        }



-- =========================================
-- MESSAGES
-- =========================================


type Msg
    = ToggledStatus Id
    | ToggledImportant Id
    | AskedToDelete Id
    | ConfirmedDelete Id
    | CanceledDelete
    | UpdatedDraft String
    | SetFilter Filter
    | CreatedTodo
    | StartedEditingTodoText Id String
    | UpdatedEditingDraft String
    | SavedEditedTodoText
    | CanceledEdit
    | NoOp
