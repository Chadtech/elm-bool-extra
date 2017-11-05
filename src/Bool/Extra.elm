module Bool.Extra
    exposing
        ( all
        , allPass
        , any
        , anyPass
        , ternary
        , none
        , notAll
        )

{-| Convenience functions for working with Bools


# Basics

@docs all, none, any, notAll


# Util

@docs conditional


# Predicate

@docs allPass, anyPass

-}


{-| All the bools are true.

    all [ True, True ] == True
    all [ True, False ] == False
    all [ False, False ] == False

-}
all : List Bool -> Bool
all =
    List.all identity


{-| None of the bools are true.

    none [ True, True ] == False
    none [ True, False ] == False
    none [ False, False ] == True

-}
none : List Bool -> Bool
none =
    List.all not


{-| At least one of the bools is true.

    any [ True, True ] == True
    any [ True, False ] == True
    any [ False, False ] == False

-}
any : List Bool -> Bool
any =
    List.any identity


{-| Not all of them are true

    notAll [ True, True ] == False
    notAll [ True, False ] == True
    notAll [ False, False ] == True

-}
notAll : List Bool -> Bool
notAll =
    all >> not


{-| A substitue for the "if" statement that can be used in one line, much like the conditional operator in Javascript

    columnStyle : Maybe Int -> Int -> List (Attribute Msg)
    columnStyle selectedColumn thisColumn =
        selectedColumn
            |> Maybe.map ((==) thisColumn)
            |> Maybe.withDefault False
            |> ternary column.focus NoFocus
            |> focusStyle

    focusStyle : Column.Focus -> List (Attribute Msg)
    focusStyle focus =
        case focus of
            NoFocus ->
                []

            People ->
                [ class "people-focused" ]

            Prices ->
                [ class "prices-focused" ]

-}
ternary : a -> a -> Bool -> a
ternary a b condition =
    if condition then
        a
    else
        b


{-| Determine if all predicates are satisfied by the value.

    allPass [(>) 20, (<) 10] 11 == True
    allPass [(>) 20, (<) 10] 21 == False
    allPass [(>) 20, (<) 10] 4 == False
    allPass [] 21 == True

-}
allPass : List (a -> Bool) -> a -> Bool
allPass ps x =
    List.foldl (\p acc -> acc && p x) True ps


{-| Determine if any predicate is satisfied by the value.

    anyPass [(>) 20, (<) 10] 100 == True
    anyPass [(>) 20, (==) 10] 21 == False
    anyPass [] 21 == False

-}
anyPass : List (a -> Bool) -> a -> Bool
anyPass ps x =
    List.foldl (\p acc -> acc || p x) False ps
