module NonNegativeInt exposing (NonNegativeInt, fromInt, toInt, zero)


type NonNegativeInt
    = NonNegativeInt Int


zero : NonNegativeInt
zero =
    NonNegativeInt 0


fromInt : Int -> Maybe NonNegativeInt
fromInt n =
    if n >= 0 then
        Just (NonNegativeInt n)

    else
        Nothing


toInt : NonNegativeInt -> Int
toInt (NonNegativeInt n) =
    n
