module Bool.Extra exposing
    ( all, none, any, notAll
    , allPass, anyPass
    , toMaybe
    , toString, fromString, stringDecoder, encodeAsString
    )

{-| Convenience functions for working with Bools


# Basics

@docs all, none, any, notAll


# Predicate

@docs allPass, anyPass


# Maybe

@docs toMaybe


# String

@docs toString, fromString, stringDecoder, encodeAsString

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| Take a value, and wrap it with `Just` from a `Bool`

    toMaybe 4 True
    --> Just 4

    toMaybe 4 False
    --> Nothing

-}
toMaybe : a -> Bool -> Maybe a
toMaybe v bool =
    if bool then
        Just v

    else
        Nothing


{-| Turn a bool into a string

    toString True
    --> "True"

    toString False
    --> "False"

-}
toString : Bool -> String
toString bool =
    if bool then
        "True"

    else
        "False"


{-| Try and extract a `Bool` from a `String`

    fromString "true"
    --> Just True

    fromString "False"
    --> Just False

    fromString "t"
    --> Nothing

    fromString "My pal foot foot"
    --> Nothing

-}
fromString : String -> Maybe Bool
fromString str =
    case String.toLower str of
        "true" ->
            Just True

        "false" ->
            Just False

        _ ->
            Nothing


{-| Sometimes in weird unideal circumstances you need to encode `True` to `"true"` instead of just `true`.

    import Json.Encode exposing (encode)

    encode 0 (encodeAsString True)
    --> "\"true\""

    encode 0 (encodeAsString False)
    --> "\"false\""

-}
encodeAsString : Bool -> Encode.Value
encodeAsString =
    Encode.string << String.toLower << toString


{-| Sometimes webservers will return the unideal json of a string `"true"` rather than just the native boolean value `true`. This decoder decodes a string that looks like a `Bool`, into a `Bool`

    import Json.Decode as Decode
    import Json.Encode as Encode

    Decode.decodeString stringDecoder "\"true\""
    --> Ok True

    Decode.decodeString stringDecoder "true"
    --> Err (Decode.Failure "Expecting a STRING" (Encode.bool True))

-}
stringDecoder : Decoder Bool
stringDecoder =
    let
        decoderFromString : String -> Decoder Bool
        decoderFromString str =
            case fromString str of
                Just bool ->
                    Decode.succeed bool

                Nothing ->
                    Decode.fail ("string is not \"true\" or \"false\" ->" ++ str)
    in
    Decode.string
        |> Decode.andThen decoderFromString


{-| All the bools are true.

    all [ True, True, True ]
    --> True

    all [ True, False ]
    --> False

    all [ False, False ]
    --> False

-}
all : List Bool -> Bool
all =
    List.all identity


{-| None of the bools are true.

    none [ True, True ]
    --> False

    none [ True, False ]
    --> False

    none [ False, False ]
    --> True

-}
none : List Bool -> Bool
none =
    List.all not


{-| At least one of the bools is true.

    any [ True, True ]
    --> True

    any [ True, False ]
    --> True

    any [ False, False ]
    --> False

-}
any : List Bool -> Bool
any =
    List.any identity


{-| Not all of them are true

    notAll [ True, True ]
    --> False

    notAll [ True, False ]
    --> True

    notAll [ False, False ]
    --> True

-}
notAll : List Bool -> Bool
notAll =
    all >> not


{-| Determine if all predicates are satisfied by the value.

    allPass [ (>) 20, (<) 10 ] 11
    --> True

    allPass [ (>) 20, (<) 10 ] 21
    --> False

    allPass [ (>) 20, (<) 10 ] 4
    --> False

    allPass [] 21
    --> True

-}
allPass : List (a -> Bool) -> a -> Bool
allPass ps x =
    List.foldl (\p acc -> acc && p x) True ps


{-| Determine if any predicate is satisfied by the value.

    anyPass [ (>) 20, (<) 10 ] 100
    --> True

    anyPass [ (>) 20, (==) 10 ] 21
    --> False

    anyPass [] 21
    --> False

-}
anyPass : List (a -> Bool) -> a -> Bool
anyPass ps x =
    List.foldl (\p acc -> acc || p x) False ps
