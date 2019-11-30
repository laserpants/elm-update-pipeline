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


testMap : Test
testMap =
    let
        model =
            41

        ( newModel, cmd ) =
            map ((+) 1) (pure model)
    in
    describe "map"
        [ test "expect value in tuple to increment by 1" <|
            \_ -> Expect.equal newModel (model + 1)
        ]


testAp : Test
testAp =
    let
        f x y =
            x ++ y

        a =
            "palak"

        b =
            "paneer"

        ( result, _ ) =
            ap (map f (pure a)) (pure b)
    in
    describe "ap"
        [ test "expect result to appear in first component" <|
            \_ -> Expect.equal result (f a b)
        ]


testMap2 : Test
testMap2 =
    let
        a =
            5

        b =
            8

        fun x y =
            x + y

        ( result, _ ) =
            map2 fun (pure a) (pure b)
    in
    describe "map2"
        [ test "expect result to match unlifted version of function" <|
            \_ -> Expect.equal result (fun a b)
        ]


testMap3 : Test
testMap3 =
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
            map3 fun (pure a) (pure b) (pure c)
    in
    describe "map3"
        [ test "expect result to match unlifted version of function" <|
            \_ -> Expect.equal result (fun a b c)
        ]


testMap4 : Test
testMap4 =
    let
        a =
            5

        b =
            8

        c =
            2

        d =
            11

        fun x y z z1 =
            x + z * y - z1

        ( result, _ ) =
            map4 fun (pure a) (pure b) (pure c) (pure d)
    in
    describe "map4"
        [ test "expect result to match unlifted version of function" <|
            \_ -> Expect.equal result (fun a b c d)
        ]


testMap5 : Test
testMap5 =
    let
        a =
            5

        b =
            8

        c =
            2

        d =
            11

        e =
            3

        fun x y z z1 z2 =
            (x + z * y - z1) ^ z2

        ( result, _ ) =
            map5 fun (pure a) (pure b) (pure c) (pure d) (pure e)
    in
    describe "map5"
        [ test "expect result to match unlifted version of function" <|
            \_ -> Expect.equal result (fun a b c d e)
        ]


testMap6 : Test
testMap6 =
    let
        a =
            5

        b =
            8

        c =
            2

        d =
            11

        e =
            3

        f =
            "tofu"

        fun x y z z1 z2 z3 =
            ( z3, (x + z * y - z1) ^ z2 )

        ( result, _ ) =
            map6 fun (pure a) (pure b) (pure c) (pure d) (pure e) (pure f)
    in
    describe "map6"
        [ test "expect result to match unlifted version of function" <|
            \_ -> Expect.equal result (fun a b c d e f)
        ]


testMap7 : Test
testMap7 =
    let
        a =
            5

        b =
            8

        c =
            2

        d =
            11

        e =
            3

        f =
            "palak"

        g =
            "paneer"

        fun x y z z1 z2 z3 z4 =
            ( z3 ++ z4, (x + z * y - z1) ^ z2 )

        ( result, _ ) =
            map7 fun (pure a) (pure b) (pure c) (pure d) (pure e) (pure f) (pure g)
    in
    describe "map7"
        [ test "expect result to match unlifted version of function" <|
            \_ -> Expect.equal result (fun a b c d e f g)
        ]


testAndMap : Test
testAndMap =
    let
        f x y z z1 =
            x + y + z - z1

        a =
            5

        b =
            6

        c =
            7

        d =
            3

        ( result, _ ) =
            map f (pure a)
                |> andMap (pure b)
                |> andMap (pure c)
                |> andMap (pure d)
    in
    describe "andMap"
        [ test "expect result to appear in first component" <|
            \_ -> Expect.equal result (f a b c d)
        ]


testJoin : Test
testJoin =
    let
        ( newModel, _ ) =
            join (pure (pure 5))
    in
    describe "join"
        [ test "expect 5 to appear in tuple" <|
            \_ -> Expect.equal newModel 5
        ]


testAndThen : Test
testAndThen =
    let
        incrementValue x =
            pure (x + 1)

        ( result, _ ) =
            pure 5
                |> andThen (\_ -> pure 7)
                |> andThen incrementValue
    in
    describe "andThen"
        [ test "expect the pipeline to return 7" <|
            \_ -> Expect.equal result 8
        ]


testKleisli : Test
testKleisli =
    let
        f x =
            pure (x + 8)

        g y =
            pure (y / 2)

        h =
            kleisli f g

        ( result, _ ) =
            h 10
    in
    describe "kleisli"
        [ test "expect the result to be " <|
            \_ -> Expect.equal result (10 / 2 + 8)
        ]


testSequence : Test
testSequence =
    let
        cmd1 =
            [ always (pure 1)
            , always (pure 3)
            ]

        cmd2 =
            [ \x -> pure (x / 2)
            , \y -> pure (y + 1)
            ]

        ( result1, _ ) =
            sequence cmd1 5

        ( result2, _ ) =
            sequence cmd2 8
    in
    describe "sequence"
        [ test "expect the result to be 3" <|
            \_ -> Expect.equal result1 3
        , test "expect the result of 8/2+1 to be 5" <|
            \_ -> Expect.equal result2 5
        ]


testAddCmd : Test
testAddCmd =
    let
        cmd =
            Cmd.map (always 1) Cmd.none

        ( _, newCmd ) =
            5
                |> addCmd cmd
    in
    describe "addCmd"
        [ test "expect cmd to appear in tuple" <|
            \_ -> Expect.equal newCmd cmd
        ]


testWith : Test
testWith =
    let
        record =
            { this = "that"
            , wat = "not"
            }

        result =
            with .this (\str rec -> { rec | wat = str ++ "hat" }) record
    in
    describe "with"
        [ test "expect second field to be concatenate string" <|
            \_ -> Expect.equal result.wat "thathat"
        ]


testUsing : Test
testUsing =
    let
        record =
            { this = "that"
            , wat = "not"
            }

        result =
            using (\{ this } _ -> this ++ "hat") record
    in
    describe "using"
        [ test "expect the result to concatenated string" <|
            \_ -> Expect.equal result "thathat"
        ]


testWhen : Test
testWhen =
    let
        f x =
            when (x > 5) (\y -> pure (y + 1))

        ( result1, _ ) =
            f 11 3

        ( result2, _ ) =
            f 4 3
    in
    describe "when"
        [ test "expect the result to be 4" <|
            \_ -> Expect.equal result1 4
        , test "expect the result to be 3" <|
            \_ -> Expect.equal result2 3
        ]


testAndWith : Test
testAndWith =
    let
        record =
            { this = "that"
            , wat = "not"
            }

        ( result, _ ) =
            pure record
                |> andWith .this (\str rec -> pure { rec | wat = str ++ "hat" })
    in
    describe "andWith"
        [ test "expect second field to be concatenate string" <|
            \_ -> Expect.equal result.wat "thathat"
        ]


testAndUsing : Test
testAndUsing =
    let
        record =
            { this = "that"
            , wat = "not"
            }

        ( result, _ ) =
            pure record
                |> andUsing (\{ this } _ -> pure (this ++ "hat"))
    in
    describe "andUsing"
        [ test "expect the result to concatenated string" <|
            \_ -> Expect.equal result "thathat"
        ]


testAndIf : Test
testAndIf =
    let
        f x =
            pure 3
                |> andIf (x > 5) (\y -> pure (y + 2))

        ( result1, _ ) =
            f 11

        ( result2, _ ) =
            f 4
    in
    describe "testAndIf"
        [ test "expect the result to be 5" <|
            \_ -> Expect.equal result1 5
        , test "expect the result to be 3" <|
            \_ -> Expect.equal result2 3
        ]


suite : Test
suite =
    describe "Update.Pipeline"
        [ testPure
        , testMap
        , testAp
        , testMap2
        , testMap3
        , testMap4
        , testMap5
        , testMap6
        , testMap7
        , testAndMap
        , testJoin
        , testAndThen
        , testKleisli
        , testSequence
        , testAddCmd
        , testWith
        , testUsing
        , testWhen
        , testAndWith
        , testAndUsing
        , testAndIf
        ]
