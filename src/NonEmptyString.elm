module NonEmptyString exposing
    ( NonEmptyString
    , fromString
    , isValid
    , toString
    )


type NonEmptyString
    = NonEmptyString String


fromString : String -> Maybe NonEmptyString
fromString str =
    if str |> String.trim |> String.isEmpty then
        Nothing

    else
        Just (NonEmptyString str)


toString : NonEmptyString -> String
toString (NonEmptyString str) =
    str


isValid : String -> Bool
isValid str =
    case fromString str of
        Just _ ->
            True

        Nothing ->
            False
