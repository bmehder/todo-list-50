module Types exposing (Editing(..), Filter(..), Id, Model, Msg(..), Status(..), Task, Todo)

import NonEmptyString exposing (NonEmptyString)
import NonNegative exposing (NonNegative)


type alias Model =
    { todos : List Todo
    , draft : String
    , filter : Filter
    , editing : Editing
    , pendingDelete : Maybe Id
    }


type alias Todo =
    { id : Id
    , task : Task
    , status : Status
    , important : Bool
    }


type alias Task =
    NonEmptyString


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
    | EditingTask
        { id : Id
        , draft : String
        }


type alias Id =
    NonNegative


type Msg
    = ToggledStatus Id
    | ToggledImportant Id
    | AskedToDelete Id
    | ConfirmedDelete Id
    | CanceledDelete
    | UpdatedDraft String
    | SetFilter Filter
    | CreatedTodo
    | StartedEditingTask Id String
    | UpdatedEditingDraft String
    | SavedEditedTask
    | CanceledEdit
    | NoOp
