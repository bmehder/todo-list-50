module Utils exposing (when)

when : (a -> Bool) -> (a -> a) -> a -> a
when predicate fn value =
    if predicate value then
        fn value
    else
        value