module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple3)
import Bool.Extra exposing (..)


all : Test
all =
    describe "Bool.Extra"
        []