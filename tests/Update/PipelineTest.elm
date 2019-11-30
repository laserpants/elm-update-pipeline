module Update.PipelineTest exposing (..)

import Update.Pipeline exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


testPure : Test
testPure =
    Debug.todo ""


suite : Test
suite =
    describe "Burrito Update"
        [ testPure
        ]

