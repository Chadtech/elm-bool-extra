module Bool.Extra
    exposing
        ( all
        , none
        , any
        , notAll
        )


{-| Convenience functions for working with Bools

# Basics

@docs all, none, any, notAll

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
    List.all (identity >> not)

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

