module Filter exposing (..)

import Types exposing (Filter(..), Status(..), Todo)


allFilters : List Filter
allFilters =
    [ All, ActiveOrImportantOnly, CompletedOnly, ImportantOnly ]


filterToString : Filter -> String
filterToString filter =
    case filter of
        All ->
            "All"

        ActiveOrImportantOnly ->
            "Active"

        CompletedOnly ->
            "Completed"

        ImportantOnly ->
            "Important"


applyFilter : Filter -> List Todo -> List Todo
applyFilter filterType =
    case filterType of
        All ->
            identity

        ActiveOrImportantOnly ->
            List.filter (\todo -> todo.status == Active || todo.important)

        CompletedOnly ->
            List.filter (.status >> (==) Completed)

        ImportantOnly ->
            List.filter .important
