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

