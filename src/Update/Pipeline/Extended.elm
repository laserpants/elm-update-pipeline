module Update.Pipeline.Extended exposing
    ( Extended, Stack, Run
    , call, sequenceCalls, runStack, runStackE, extend, mapE, mapE2, mapE3
    , lift, lift2, lift3
    , andCall, andLift
    )

{-| This module introduces a simple callback utility that enables information to be passed upwards in the model-update hierarchy.
The pattern is similar to event handling in object-oriented languages, in the sense that we can think of a nested model as an _event source_, and of the parent as a _listener_. The event listener is able to respond to state changes by hooking into the event source via one or more callbacks (event handlers).


## Usage example:

The below example shows how to use the [`Extended`](#Extended) type alias and [`runStack`](#runStack) to implement an `update` function which supports callbacks.

Scroll down for a more detailed explanation.

    module Main exposing (..)

    import Browser exposing (Document, document)
    import Html exposing (Html, button, span, text)
    import Html.Attributes as Attributes
    import Html.Events exposing (onClick)
    import Update.Pipeline exposing (..)
    import Update.Pipeline.Extended exposing (..)

    type ChildMsg
        = OnClick Bool

    type alias Child =
        {}

    initChild : ( Child, Cmd ChildMsg )
    initChild =
        save {}

    {- #1 -}
    updateChild :
        ChildMsg
        -> { onClick : Bool -> a }
        -> Extended Child a
        -> ( Extended Child a, Cmd ChildMsg )
    updateChild msg { onClick } model =
        case msg of
            OnClick choice ->
                {- #2 -}
                model
                    |> call (onClick choice)

    viewChild : Child -> Html ChildMsg
    viewChild _ =
        span []
            [ button
                [ onClick (OnClick True) ]
                [ text "Yay" ]
            , button
                [ onClick (OnClick False) ]
                [ text "Nay" ]
            ]

    type Msg
        = ChildMsg ChildMsg

    type alias Parent =
        { child : Child
        , clicked : Bool
        , choice : Maybe Bool
        }

    insertAsChildIn : Parent -> Child -> ( Parent, Cmd Msg )
    insertAsChildIn model child =
        save { model | child = child }

    {- #3 -}
    inChild : Run Parent Child Msg ChildMsg a
    inChild =
        runStack .child insertAsChildIn ChildMsg

    init : () -> ( Parent, Cmd Msg )
    init () =
        map3 Parent
            (mapCmd ChildMsg initChild)
            (save False)
            (save Nothing)

    {- #4 -}
    handleClick :
        Bool
        -> Parent
        -> ( Parent, Cmd Msg )
    handleClick choice model =
        save
            { model
                | clicked = True
                , choice = Just choice
            }

    update : Msg -> Parent -> ( Parent, Cmd Msg )
    update msg model =
        case msg of
            ChildMsg childMsg ->
                {- #5 -}
                model
                    |> inChild
                        (updateChild childMsg
                            { onClick = handleClick }
                        )

    subscriptions : Parent -> Sub Msg
    subscriptions _ =
        Sub.none

    view : Parent -> Document Msg
    view { child, clicked, choice } =
        { title = ""
        , body =
            [ if clicked then
                let
                    choiceStr =
                        case choice of
                            Just True ->
                                "yay"

                            _ ->
                                "nay"
                in
                text ("You chose " ++ choiceStr ++ "!")

              else
                Html.map ChildMsg (viewChild child)
            ]
        }

    main : Program () Parent Msg
    main =
        document
            { init = init
            , update = update
            , subscriptions = subscriptions
            , view = view
            }


### Explanation:

1.  This `update` function is atypical in the following ways:
      - Instead of

            m -> ( m, Cmd msg )

        … we change the type so that it ends with

            Extended m a -> ( Extended m a, Cmd msg )

      - The second argument is a record containing a callback

            onClick : Bool -> a

        In general, we can have any number of these functions. The type of a callback is always of the form

            arg1 -> arg2 -> ... -> a

2.  `call` adds a function to the list of callbacks that is returned together with the model.

3.  Partially applied, [`runStack`](#runStack) gives us a function that takes care of updating the nested `Child` model.
    extra structure needed for callbacks.
    We provide `runStack` with a getter (`.child`), a setter (`insertAsChildIn`), and the `Msg` constructor (`ChildMsg`).

        getter : Parent -> Child

        setter : Parent -> Child -> ( Parent, Cmd Msg )

        toMsg : ChildMsg -> Msg

    The actual type of the resulting function is slightly complicated, so we'll typically use the [`Run`](#Run) alias to make things more readable.

4.  The handler's type has to match that of the callback.

        Bool -> a

    becomes

        Bool -> Parent -> ( Parent, Cmd Msg )

5.  In the parent model's `update` function, we use the `inChild` helper (see #3) to update the nested model.


# Types

@docs Extended, Stack, Run


# Callback Interface

@docs call, sequenceCalls, runStack, runStackE, extend, mapE, mapE2, mapE3

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


{-| Take an effectful update function `(a -> ( b, Cmd msg ))` and transform it into one that instead operates on an `Extended a c` value and returns an `( Extended b c, Cmd msg )` pair.

See also [`andLift`](#andLift).

_Aside:_ This function behaves like `traverse` in Haskell, when we think of updates (`a -> ( b, Cmd msg )`) as monadic functions `a -> m b`.

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

See also [`andCall`](#andCall).

-}
call : c -> Extended a c -> ( Extended a c, Cmd msg )
call =
    addCalls << List.singleton


{-| Shortcut for `andThen <<`[`call`](#call).
-}
andCall : c -> ( Extended a c, Cmd msg ) -> ( Extended a c, Cmd msg )
andCall =
    andThen << call


{-| Compose and apply the list monadic functions (callbacks) accumulated by a nested update call.
Usually it is not necessary to use this function directly in client code.
Instead, see [`runStack`](#runStack).

This function is identical to:

    uncurry (flip sequence)

See also [`sequence`](Update.Pipeline#sequence) in `Update.Pipeline`.

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


{-| This function provides the glue code necessary to update a nested model, and subsequently run any resulting callbacks on the parent model.


#### _Example:_

    updateFeature :
        FeatureMsg
        -> { onComplete : Message -> a
           , onError : Error -> a
           }
        -> Extended Feature
        -> ( Extended Feature, Cmd FeatureMsg )
    updateFeature =
        -- ...

    type Msg
        = FeatureMsg FeatureMsg

    type alias Parent =
        { feature : Feature
        }

    inFeature : Run Parent Feature Msg FeatureMsg a
    inFeature =
        runStack
            .feature
            (\m feature -> save { m | feature = feature })
            FeatureMsg

    update : Msg -> Parent -> ( Parent, Cmd Msg )
    update msg model =
        case msg of
            FeatureMsg featureMsg ->
                inFeature (updateFeature featureMsg)

See also [`Run`](#Run), [`runStackE`](#runStackE).

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
