module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple3)
import Bool.Extra exposing (..)


all : Test
all =
    describe "Bool.Extra"
        [ describe "Bool.Extra.all" (basicsTest Bool.Extra.all [ True, False, False ])
        , describe "Bool.Extra.none" (basicsTest Bool.Extra.none [ False, False, True ])
        , describe "Bool.Extra.any" (basicsTest Bool.Extra.any [ True, True, False ])
        , describe "Bool.Extra.notAll" (basicsTest Bool.Extra.notAll [ False, True, True ])
        , describe "allPass"
            [ test "all predicates are satisfied by the value" <|
                \() ->
                    Expect.true "11 is greater than 10 and less than 20" (allPass [ (>) 20, (<) 10 ] 11)
            , test "without any predicates" <|
                \() ->
                    Expect.true "no predicate is not satisfied" (allPass [] 21)
            , test "the first predicate is not satisfied by the value" <|
                \() ->
                    Expect.false "21 is greater than 20" (allPass [ (>) 20, (<) 10 ] 21)
            , test "the second predicate is not satisfied by the value" <|
                \() ->
                    Expect.false "4 is less than 10" (allPass [ (>) 20, (<) 10 ] 4)
            ]
        , describe "anyPass"
            [ test "at least one predicate is satisfied by the value" <|
                \() ->
                    Expect.true "100 is greater than 10" (anyPass [ (>) 20, (<) 10 ] 100)
            , test "none of the predicates are satisfied by the value" <|
                \() ->
                    Expect.false "21 is not less than 20 and not equals 10 " (anyPass [ (>) 20, (==) 10 ] 21)
            , test "without any predicates" <|
                \() ->
                    Expect.false "no predicate is satisfied" (anyPass [] 21)
            ]
        ]


basicsTest : (List Bool -> Bool) -> List Bool -> List Test
basicsTest f values =
    threeCases
        |> List.map (areEqualBasics f)
        |> List.map2 (|>) values


areEqualBasics : (List Bool -> Bool) -> List Bool -> Bool -> Test
areEqualBasics f bools bool =
    test (makeDescription bools bool) <|
        \_ ->
            Expect.equal (f bools) bool


makeDescription : List Bool -> Bool -> String
makeDescription bools bool =
    (toString bools) ++ " is " ++ (toString bool)


threeCases : List (List Bool)
threeCases =
    [ [ True, True, True ]
    , [ True, True, False ]
    , [ False, False, False ]
    ]
