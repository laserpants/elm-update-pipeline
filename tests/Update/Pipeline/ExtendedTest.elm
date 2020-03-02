module Update.Pipeline.ExtendedTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Update.Pipeline exposing (andThen, save)
import Update.Pipeline.Extended exposing (..)


testExtend : Test
testExtend =
    let
        model =
            5

        ( newModel, calls ) =
            extend model
    in
    describe "extend"
        [ test "expect 5 to appear in tuple"
            (\_ -> Expect.equal model newModel)
        , test "expect no calls"
            (\_ -> Expect.equal [] calls)
        ]


testMapE : Test
testMapE =
    let
        model =
            41

        ( newModel, calls ) =
            mapE ((+) 1) (extend model)
    in
    describe "mapE"
        [ test "expect value in tuple to increment by 1"
            (\_ -> Expect.equal newModel (model + 1))
        ]


testMapE2 : Test
testMapE2 =
    let
        a =
            5

        b =
            8

        fun x y =
            x + y

        ( result, calls ) =
            mapE2 fun (extend a) (extend b)
    in
    describe "mapE2"
        [ test "expect result to match unlifted version of function"
            (\_ -> Expect.equal result (fun a b))
        ]


testMapE3 : Test
testMapE3 =
    let
        a =
            5

        b =
            8

        c =
            2

        fun x y z =
            x + z * y

        ( result, _ ) =
            mapE3 fun (extend a) (extend b) (extend c)
    in
    describe "mapE3"
        [ test "expect result to match unlifted version of function"
            (\_ -> Expect.equal result (fun a b c))
        ]


testLift : Test
testLift =
    let
        fun a =
            save (a + 2)

        ( val, cmd ) =
            lift fun (extend 3)
    in
    describe "lift"
        [ test "expect lift to preserve behavior of original function"
            (\_ -> Expect.equal val (extend 5))
        ]


testLift2 : Test
testLift2 =
    let
        fun a b =
            save (a + b)

        ( val, cmd ) =
            lift2 fun (extend 5) (extend 8)
    in
    describe "lift2"
        [ test "expect lift2 to preserve behavior of original function"
            (\_ -> Expect.equal val (extend 13))
        ]


testLift3 : Test
testLift3 =
    let
        fun x y z =
            save (x + z * y)

        ( val, cmd ) =
            lift3 fun (extend 5) (extend 8) (extend 2)
    in
    describe "lift3"
        [ test "expect lift3 to preserve behavior of original function"
            (\_ -> Expect.equal val (extend 21))
        ]


testAndLift : Test
testAndLift =
    let
        fun a =
            save (a + 2)

        ( val, cmd ) =
            save (extend 8)
                |> andThen (lift fun)
    in
    describe "andLift"
        [ test "expect andLift to preserve behavior of original function"
            (\_ -> Expect.equal val (extend 10))
        ]


testCall : Test
testCall =
    let
        ( result, _ ) =
            extend 1
                |> call (save 2)
    in
    describe "call"
        [ test "expect call to add function to list of callbacks"
            (\_ -> Expect.equal result ( 1, [ save 2 ] ))
        ]


testAndCall : Test
testAndCall =
    let
        ( result, _ ) =
            extend 1
                |> save
                |> andCall (save 2)
    in
    describe "andCall"
        [ test "expect andCall to add function to list of callbacks"
            (\_ -> Expect.equal result ( 1, [ save 2 ] ))
        ]


testSequenceCalls : Test
testSequenceCalls =
    let
        fun1 { multiplyBy, incrementBy } msg state =
            extend state
                |> call (multiplyBy 2)
                |> andCall (incrementBy 3)

        ( result, _ ) =
            fun1
                { multiplyBy = \a s -> save (s * a)
                , incrementBy = \a s -> save (s + a)
                }
                ()
                3
                |> andThen sequenceCalls
    in
    describe "sequenceCalls"
        [ test "expected sequenceCalls to apply callbacks to parent model"
            (\_ -> Expect.equal result 9)
        ]


type alias TestModel2 =
    { count : Int
    }


type alias TestModel1 =
    { m : TestModel2
    }


testRunStack : Test
testRunStack =
    let
        inNested =
            runStack .m (\m0 m1 -> save { m0 | m = m1 }) identity

        update2 _ ( model, calls ) =
            save
                ( { model | count = model.count + 1 }, calls )

        update1 _ model =
            model
                |> inNested (update2 ())

        ( { m }, _ ) =
            update1 () { m = { count = 1 } }
    in
    describe "runStack"
        [ test "expect runStack to update nested model"
            (\_ -> Expect.equal m.count 2)
        ]


testRunStackE : Test
testRunStackE =
    let
        inNested =
            runStackE .m (\m0 m1 -> save { m0 | m = m1 }) identity

        update2 _ ( model, calls ) =
            save
                ( { model | count = model.count + 3 }, calls )

        update1 _ model =
            model
                |> inNested (update2 ())

        ( ( { m }, _ ), _ ) =
            update1 () (extend { m = { count = 1 } })
    in
    describe "runStackE"
        [ test "expect runStackE to update nested model"
            (\_ -> Expect.equal m.count 4)
        ]


testChoosing : Test
testChoosing =
    let
        record =
            { this = "that"
            , wat = "not"
            }

        result =
            choosing (\{ this } _ -> this ++ "hat") (extend record)
    in
    describe "using"
        [ test "expect the result to be the concatenated string"
            (\_ -> Expect.equal result "thathat")
        ]


suite : Test
suite =
    describe "Update.Pipeline.Extended"
        [ testExtend
        , testMapE
        , testMapE2
        , testMapE3
        , testLift
        , testLift2
        , testLift3
        , testAndLift
        , testCall
        , testAndCall
        , testSequenceCalls
        , testRunStack
        , testRunStackE
        , testChoosing
        ]
