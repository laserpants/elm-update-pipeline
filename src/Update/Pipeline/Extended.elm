module Update.Pipeline.Extended exposing
    ( Extended, Stack, Run
    , call, sequenceCalls, runStack, runStackE
    , extend, mapE, mapE2, mapE3
    , lift, lift2, lift3
    , andCall, andLift
    )

{-| This module introduces a simple callback mechanism that enables information to be passed upwards in the model-update hierarchy.
The pattern is similar to callback-based event handling in object-oriented languages, in the sense that we can think of a nested model as an _event source_, and of the parent as a _listener_. The event listener can respond to state changes by hooking into the event source via one or more callbacks (event handlers).


### Basic use:

abc


# Types

@docs Extended, Stack, Run


# Program Integration

@docs call, sequenceCalls, runStack, runStackE


# Functor and Applicative Interface

@docs extend, mapE, mapE2, mapE3


# Traversing

@docs lift, lift2, lift3


# Shortcuts

@docs andCall, andLift

-}

import Tuple exposing (mapFirst)
import Update.Pipeline exposing (andThen, mapCmd, save, sequence)


{-| An _extended_ model is a model paired with a list of _callbacks_ — functions that are applied to the parent model after an update.
-}
type alias Extended m a =
    ( m, List a )


{-| Create an extended model without any callbacks.
-}
extend : a -> Extended a c
extend model =
    ( model, [] )


shuffle : Extended ( a, Cmd msg ) c -> ( Extended a c, Cmd msg )
shuffle ( ( model, cmd ), calls ) =
    ( ( model, calls ), cmd )


{-| Map a function `a -> b` over an `Extended a *` model.
-}
mapE : (a -> b) -> Extended a c -> Extended b c
mapE f ( x, calls ) =
    ( f x, calls )


{-| A version of [`mapE`](#mapE) that accepts functions of two arguments as input.
-}
mapE2 : (a -> b -> c) -> Extended a d -> Extended b d -> Extended c d
mapE2 f ( x, calls1 ) ( y, calls2 ) =
    ( f x y, calls1 ++ calls2 )


{-| A version of [`mapE`](#mapE) that accepts functions of three arguments as input.
-}
mapE3 : (a -> b -> c -> d) -> Extended a e -> Extended b e -> Extended c e -> Extended d e
mapE3 f ( x, calls1 ) ( y, calls2 ) ( z, calls3 ) =
    ( f x y z, calls1 ++ calls2 ++ calls3 )


{-| Take an effectful update function `(a -> ( b, Cmd msg ))` and transform it into one that instead operates on an `Extended a c` value.

_Aside:_ This function behaves just like mapM (or traverse) in Haskell,
building on the idea of updates (`a -> ( b, Cmd msg )`) being monadic functions.
monadic value comes into form of a model-cmd pair:

-}
lift :
    (a -> ( b, Cmd msg ))
    -> Extended a c
    -> ( Extended b c, Cmd msg )
lift fun a =
    shuffle (mapE fun a)


{-| A version of [`lift`](#lift) for functions of two arguments.
-}
lift2 :
    (a -> b -> ( c, Cmd msg ))
    -> Extended a d
    -> Extended b d
    -> ( Extended c d, Cmd msg )
lift2 fun a b =
    shuffle (mapE2 fun a b)


{-| A version of [`lift`](#lift) for functions of three arguments.
-}
lift3 :
    (a -> b -> c -> ( d, Cmd msg ))
    -> Extended a e
    -> Extended b e
    -> Extended c e
    -> ( Extended d e, Cmd msg )
lift3 fun a b c =
    shuffle (mapE3 fun a b c)


{-| Shortcut for `andThen <<`[`lift`](#lift).
-}
andLift :
    (a -> ( b, Cmd msg ))
    -> ( Extended a c, Cmd msg )
    -> ( Extended b c, Cmd msg )
andLift =
    andThen << lift


addCalls : List c -> Extended a c -> ( Extended a c, Cmd msg )
addCalls calls1 ( model, calls ) =
    save ( model, calls ++ calls1 )


{-| Invoke a callback in an update function that returns an `( Extended a c, Cmd msg )` value.
-}
call : c -> Extended a c -> ( Extended a c, Cmd msg )
call =
    addCalls << List.singleton


{-| Shortcut for `andThen <<`[`call`](#call).
-}
andCall : c -> ( Extended a c, Cmd msg ) -> ( Extended a c, Cmd msg )
andCall =
    andThen << call


{-| Compose and apply the list of monadic functions (callbacks) accumulated by a nested update call.
See also [`sequence`](Update.Pipeline#sequence). This function is identical to:

    uncurry (flip sequence)

Usually it shouldn't be necessary to use this function directly in client code.
Instead, see [`runStack`](#runStack).

-}
sequenceCalls : Extended a (a -> ( a, Cmd msg )) -> ( a, Cmd msg )
sequenceCalls ( model, calls ) =
    sequence calls model


{-| Represents an update where callbacks may be present.
-}
type alias Stack m m1 msg msg1 a =
    Extended m1 a -> ( Extended m1 (m -> ( m, Cmd msg )), Cmd msg1 )


{-| An alias that helps making type signatures less verbose in client code.
See [`runStack`](#runStack) for an example of how to use this type alias.
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


{-| A version of [`runStack`](#runStack) that can be used to update extended models.
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


{-| TODO


#### _Example:_

    type Msg
        = ChildMsg ChildMsg

    type alias Parent =
        { child : Child
        }

    inChildModel : Run Parent Child Msg ChildMsg a
    inChildModel =
        runStack .child (\m child -> save { m | child = child }) ChildMsg

    update : Msg -> Parent -> ( Parent, Cmd Msg )
    update msg model =
        case msg of
            ChildMsg childMsg ->
                inChildModel (updateChild childMsg)

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
