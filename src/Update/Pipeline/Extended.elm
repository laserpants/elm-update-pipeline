module Update.Pipeline.Extended exposing
    ( Extended, Stack, Run
    , extend, mapE, mapE2, mapE3
    , lift, lift2, lift3
    , call
    , sequenceCalls, runStack, runStackE
    , andCall, andLift
    )

{-| This module extends the _ for working with pipelines, introducing a simple callback mechanism which allows information to be passed up in the update hierarchy.


# Types

@docs Extend, Stack, Run


# Functor

@docs extend, mapE, mapE2, mapE3


# Traversing

@docs lift, lift2, lift3


# Hello

@docs call


# What

#docs sequenceCalls, runStack, runStackE


# Shortcuts

@docs andCall, andLift

-}


import Tuple exposing (mapFirst)
import Update.Pipeline exposing (andThen, mapCmd, save, sequence)


{- | A data type that encapsulates a model of type `m` together with a list of callbacks
`m -> ( m, Cmd msg )`
????

-}
type alias Extended m a =
    ( m, List a )


{- | Create an extended model without any callbacks.

-}
extend : a -> Extended a c
extend model =
    ( model, [] )


shuffle : Extended ( a, Cmd msg ) c -> ( Extended a c, Cmd msg )
shuffle ( ( model, cmd ), calls ) =
    ( ( model, calls ), cmd )


{- | Apply a function to the model

-}
mapE : (a -> b) -> Extended a c -> Extended b c
mapE f ( x, calls ) =
    ( f x, calls )


{- | A version of `[`mapE`](#mapE) for functions of two arguments.

-}
mapE2 : (a -> b -> c) -> Extended a d -> Extended b d -> Extended c d
mapE2 f ( x, calls1 ) ( y, calls2 ) =
    ( f x y, calls1 ++ calls2 )


{- | A version of `[`mapE`](#mapE) for functions of three arguments.

-}
mapE3 : (a -> b -> c -> d) -> Extended a e -> Extended b e -> Extended c e -> Extended d e
mapE3 f ( x, calls1 ) ( y, calls2 ) ( z, calls3 ) =
    ( f x y z, calls1 ++ calls2 ++ calls3 )


{- | Take an effectful update function `(a -> ( b, Cmd msg ))` and transform it into one that instead acts on an extended model of type `Extended a c`.

Aside: This function behaves like mapM (or traverse) in Haskell
in a way consistent with
the _ of updates being monadic functions of the type `a -> ( a, Cmd msg )`.


-}
lift :
    (a -> ( b, Cmd msg ))
    -> Extended a c
    -> ( Extended b c, Cmd msg )
lift fun a =
    shuffle (mapE fun a)


{- | A version of `[`lift`](#lift) for functions of two arguments.

-}
lift2 :
    (a -> b -> ( c, Cmd msg ))
    -> Extended a d
    -> Extended b d
    -> ( Extended c d, Cmd msg )
lift2 fun a b =
    shuffle (mapE2 fun a b)


{- | A version of `[`lift`](#lift) for functions of three arguments.

-}
lift3 :
    (a -> b -> c -> ( d, Cmd msg ))
    -> Extended a e
    -> Extended b e
    -> Extended c e
    -> ( Extended d e, Cmd msg )
lift3 fun a b c =
    shuffle (mapE3 fun a b c)


{- | Shortcut for `andThen <<`[`lift`](#lift).
-}
andLift :
    (a -> ( b, Cmd msg ))
    -> ( Extended a c, Cmd msg )
    -> ( Extended b c, Cmd msg )
andLift =
    andThen << lift


addCalls : List c -> Extended a c -> ( Extended a c, Cmd msg )
addCalls calls1 ( model, calls ) =
    save ( model, calls1 ++ calls )


{- | TODO

-}
call : c -> Extended a c -> ( Extended a c, Cmd msg )
call =
    addCalls << List.singleton


{- | Shortcut for `andThen <<`[`call`](#call).
-}
andCall : c -> ( Extended a c, Cmd msg ) -> ( Extended a c, Cmd msg )
andCall =
    andThen << call


{- | TODO

-}
sequenceCalls : Extended a (a -> ( a, Cmd msg )) -> ( a, Cmd msg )
sequenceCalls ( model, calls ) =
    sequence calls model


{- | TODO

-}
type alias Stack m m1 msg msg1 a =
    Extended m1 a -> ( Extended m1 (m -> ( m, Cmd msg )), Cmd msg1 )


{- | A type alias that helps making type signature less verbose in library code.

-}
type alias Run m m1 msg msg1 a =
    Stack m m1 msg msg1 a -> m -> ( m, Cmd msg )


run :
    (Extended b c -> Extended a (a -> ( a, Cmd msg1 )))
    -> (d -> e)
    -> (d -> a1 -> ( b, Cmd msg1 ))
    -> (msg -> msg1)
    -> (Extended e f -> ( Extended a1 c, Cmd msg ))
    -> d
    -> ( a, Cmd msg1 )
run fun get set toMsg stack model =
    ( get model, [] )
        |> stack
        |> mapCmd toMsg
        |> andLift (set model)
        |> andThen (fun >> sequenceCalls)


{- | A version of `[`runStack`](#runStack) that ...

-}
runStackE :
    (d -> m1)
    -> (d -> a -> ( b, Cmd msg ))
    -> (msg1 -> msg)
    -> (Extended m1 f -> ( Extended a (Extended b c -> ( Extended b c, Cmd msg )), Cmd msg1 ))
    -> Extended d c
    -> ( Extended b c, Cmd msg )
runStackE g s m stack ( model, calls ) =
    model
        |> run (mapFirst extend) g s m stack
        |> andThen (addCalls calls)


{- | TODO

-}
runStack :
    (a -> m1)
    -> (a -> m1 -> ( m, Cmd msg ))
    -> (msg1 -> msg)
    -> Stack m m1 msg msg1 b
    -> a
    -> ( m, Cmd msg )
runStack =
    run identity
