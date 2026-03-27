module Utils exposing (applyIf)

applyIf : (a -> Bool) -> (a -> a) -> a -> a
applyIf predicate fn value =
    if predicate value then
        fn value
    else
        value