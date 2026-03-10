module NonNegative exposing (NonNegative, fromInt, toInt)

type NonNegative
    = NonNegative Int

fromInt : Int -> Maybe NonNegative
fromInt n =
    if n >= 0 then
        Just (NonNegative n)
    else
        Nothing

toInt : NonNegative -> Int
toInt (NonNegative n) =
    n