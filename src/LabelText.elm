module LabelText exposing
    ( itemsLabel
    , remainingLabel
    , completedLabel
    , importantLabel
    )

pluralize : String -> String -> Int -> String
pluralize singular plural count =
    case count of
        1 ->
            singular

        _ ->
            plural


itemsLabel : Int -> String
itemsLabel =
    pluralize " item" " items"


remainingLabel : Int -> String
remainingLabel =
    pluralize " item remaining" " items remaining"


completedLabel : Int -> String
completedLabel =
    pluralize " item completed" " items completed"


importantLabel : Int -> String
importantLabel =
    pluralize " important item" " important items"