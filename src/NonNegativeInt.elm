module NonNegativeInt exposing (NonNegativeInt, fromInt, toInt)


type NonNegativeInt
    = NonNegativeInt Int


fromInt : Int -> Maybe NonNegativeInt
fromInt n =
    if n >= 0 then
        Just (NonNegativeInt n)

    else
        Nothing


toInt : NonNegativeInt -> Int
toInt (NonNegativeInt n) =
    n
