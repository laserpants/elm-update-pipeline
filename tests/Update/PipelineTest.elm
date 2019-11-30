module Update.PipelineTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Update.Pipeline exposing (..)


testPure : Test
testPure =
    let
        model =
            5

        ( newModel, cmd ) =
            pure model
    in
    describe "pure"
        [ test "expect 5 to appear in tuple" <|
            \_ -> Expect.equal model newModel
        , test "expect no command" <|
            \_ -> Expect.equal Cmd.none cmd
        ]


testMap =
    Debug.todo ""


testAp =
    Debug.todo ""


testMap2 =
    Debug.todo ""


testMap3 =
    Debug.todo ""


testMap4 =
    Debug.todo ""


testMap5 =
    Debug.todo ""


testMap6 =
    Debug.todo ""


testMap7 =
    Debug.todo ""


testAndMap =
    Debug.todo ""


testJoin =
    Debug.todo ""


testAndThen =
    Debug.todo ""


testKleisli =
    Debug.todo ""


testSequence =
    Debug.todo ""


testAddCmd =
    Debug.todo ""


testMapCmd =
    Debug.todo ""


testAndAddCmd =
    Debug.todo ""


testWith =
    Debug.todo ""


testUsing =
    Debug.todo ""


testWhen =
    Debug.todo ""


testAndWith =
    Debug.todo ""


testAndUsing =
    Debug.todo ""


testAndIf =
    Debug.todo ""


suite : Test
suite =
    describe "Burrito Update"
        [ testPure
        ]
