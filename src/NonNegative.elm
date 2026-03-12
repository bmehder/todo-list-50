module NonNegative exposing (NonNegative, fromInt, toInt, zero)


type NonNegative
    = NonNegative Int


zero : NonNegative
zero =
    NonNegative 0


fromInt : Int -> Maybe NonNegative
fromInt n =
    if n >= 0 then
        Just (NonNegative n)

    else
        Nothing


toInt : NonNegative -> Int
toInt (NonNegative n) =
    n
