module Update.Pipeline.Extended exposing
    ( Extended, Stack, Run
    , call, sequenceCalls, runStack, runStackE, extend, mapE, mapE2, mapE3
    , lift, lift2, lift3
    , andCall, andLift
    )

{-| This module introduces a simple utility for information to be passed upwards in the model-update hierarchy.
The pattern is similar to event handling in object-oriented languages, in the sense that we can think of a nested model as an _event source_, and of the parent as a _listener_. The event listener is then able to respond to state changes by hooking into the event source via one or more _callbacks_ (event handlers).


## Usage example:

The following example shows how to use this module to implement an update function with support for callbacks.
Scroll down for explanations of the indicated points in the code.

    module Main exposing (..)

    import Browser exposing (Document, document)
    import Html exposing (Html, button, span, text)
    import Html.Attributes as Attributes
    import Html.Events exposing (onClick)
    import Update.Pipeline exposing (..)
    import Update.Pipeline.Extended exposing (..)

    type FeatureMsg
        = OnClick Bool

    type alias Feature =
        {}

    initFeature : ( Feature, Cmd FeatureMsg )
    initFeature =
        save {}

    {- #1 -}
    updateFeature :
        FeatureMsg
        -> { onClick : Bool -> a }
        -> Extended Feature a
        -> ( Extended Feature a, Cmd FeatureMsg )
    updateFeature msg { onClick } model =
        case msg of
            OnClick choice ->
                {- #2 -}
                model
                    |> call (onClick choice)

    viewFeature : Feature -> Html FeatureMsg
    viewFeature _ =
        span []
            [ button
                [ onClick (OnClick True) ]
                [ text "Yay" ]
            , button
                [ onClick (OnClick False) ]
                [ text "Nay" ]
            ]

    type Msg
        = FeatureMsg FeatureMsg

    type alias Model =
        { feature : Feature
        , clicked : Bool
        , choice : Maybe Bool
        }

    insertAsFeatureIn : Model -> Feature -> ( Model, Cmd Msg )
    insertAsFeatureIn model feature =
        save { model | feature = feature }

    {- #3 -}
    inFeature : Run Model Feature Msg FeatureMsg a
    inFeature =
        runStack .feature insertAsFeatureIn FeatureMsg

    init : () -> ( Model, Cmd Msg )
    init () =
        map3 Model
            (mapCmd FeatureMsg initFeature)
            (save False)
            (save Nothing)

    {- #4 -}
    handleClick :
        Bool
        -> Model
        -> ( Model, Cmd Msg )
    handleClick choice model =
        save
            { model
                | clicked = True
                , choice = Just choice
            }

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            FeatureMsg featureMsg ->
                {- #5 -}
                model
                    |> inFeature
                        (updateFeature featureMsg
                            { onClick = handleClick }
                        )

    subscriptions : Model -> Sub Msg
    subscriptions _ =
        Sub.none

    view : Model -> Document Msg
    view { feature, clicked, choice } =
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
                Html.map FeatureMsg (viewFeature feature)
            ]
        }

    main : Program () Model Msg
    main =
        document
            { init = init
            , update = update
            , subscriptions = subscriptions
            , view = view
            }


### Explanation:

1.  The update function is atypical in the following ways:
      - Instead of the usual

            m -> ( m, Cmd msg )

        … we change the type so that it ends with

            Extended m a -> ( Extended m a, Cmd msg )

      - The second argument is a record containing a callback

            onClick : Bool -> a

        In general, we can have any number of these functions. The type of a callback is always of the form

            arg1 -> arg2 -> ... -> a

2.  Using [`call`](#call), we add a function to the list of callbacks which is eventually returned together with the model. Think of this as invoking the callback.

3.  Partially applied, [`runStack`](#runStack) gives us a function that takes care of updating the nested `Feature` model in a way that also accommodates for the extra callback structure.
    The actual type of the resulting function is slightly complicated, so we’ll typically use the [`Run`](#Run) alias to make things more readable.

4.  The handler’s type has to match that of the callback.
    The type parameter `a` is expanded to `m -> ( m, Cmd msg )`, where `m` is the actual type of the parent model.
    So, in this example

        Bool -> a

    … becomes

        Bool -> Model -> ( Model, Cmd Msg )

5.  Finally, in the parent model’s update function, we use the `inFeature` helper (see #3) to update the nested model and apply the callbacks.


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


{-| A version of [`mapE`](#mapE) that takes a function of two arguments as input.
-}
mapE2 : (a -> b -> c) -> Extended a d -> Extended b d -> Extended c d
mapE2 f ( x, calls1 ) ( y, calls2 ) =
    ( f x y, calls1 ++ calls2 )


{-| A version of [`mapE`](#mapE) that takes a function of three arguments as input.
-}
mapE3 : (a -> b -> c -> d) -> Extended a e -> Extended b e -> Extended c e -> Extended d e
mapE3 f ( x, calls1 ) ( y, calls2 ) ( z, calls3 ) =
    ( f x y z, calls1 ++ calls2 ++ calls3 )


{-| Take an _effectful_ update function `(a -> ( b, Cmd msg ))` and transform it into one that instead operates on `Extended a c` values and returns an `( Extended b c, Cmd msg )` pair.

_Aside:_ This function behaves like `traverse` in the Traversable type class in Haskell, when we think of updates (`a -> ( b, Cmd msg )`) as the monadic function `a -> m b`.

See also [`andLift`](#andLift).

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


{-| Invoke a callback in an _extended_ update function. That is, one that returns an `( Extended a c, Cmd msg )` value, as opposed to the usual `( a, Cmd msg )` pair.

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


{-| Compose and apply the list of functions (callbacks) accumulated by a nested update call.
Usually it is not necessary to use this function directly in client code.
Instead, see [`runStack`](#runStack).

See also [`sequence`](Update.Pipeline#sequence) in `Update.Pipeline`.
This function is identical to `uncurry (flip sequence)`.

-}
sequenceCalls : Extended a (a -> ( a, Cmd msg )) -> ( a, Cmd msg )
sequenceCalls ( model, calls ) =
    sequence calls model


{-| Represents an update where callbacks may be present.
-}
type alias Stack m m1 msg msg1 a =
    Extended m1 a -> ( Extended m1 (m -> ( m, Cmd msg )), Cmd msg1 )


{-| An alias that helps making type signatures less verbose in client code.
See [`runStack`](#runStack) for an example of how to use this.
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


{-| A version of [`runStack`](#runStack) that can be used when both the child’s and the parent’s update functions are of the extended type.

Here is a modified version of the example from the documentation for `runStack`:

    updateInner :
        InnerMsg
        -> { onComplete : Message -> a
           , onError : Error -> a
           }
        -> Extended InnerModel
        -> ( Extended InnerModel, Cmd InnerMsg )
    updateInner =
        -- ...

    inInner : Run (Extended Model c) InnerModel Msg InnerMsg a
    inInner =
        runStackE
            .inner
            (\m inner -> save { m | inner = inner })
            InnerMsg

    update :
        Msg
        -> Extended Model c
        -> ( Extended Model c, Cmd Msg )
    update msg model =
        -- ...

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


{-| Some amount of glue code is required to update a nested model, and subsequently apply the resulting callbacks to the outer model.
`runStack` takes care of those internals. Typically, it is partially applied with the first three arguments:

  - The first argument is a function that _extracts_ the inner model from the outer.
    This is usually just a property accessor, like `.inner` in the below example.

```
getter : outer -> inner
```

  - The function in the second argument is used to _reinsert_ the inner model back into the outer (parent) model.

```
setter : outer -> inner -> ( outer, Cmd msg )
```

  - The third argument is a constructor that takes a message of the inner model’s message type as input and creates one of the parent’s type.

```
toMsg : msg1 -> msg
```


#### _Here is an example:_

    updateInner :
        InnerMsg
        -> { onComplete : Message -> a
           , onError : Error -> a
           }
        -> Extended InnerModel
        -> ( Extended InnerModel, Cmd InnerMsg )
    updateInner =
        -- ...

    type Msg
        = InnerMsg InnerMsg

    type alias Model =
        { inner : InnerModel
        }

    inInner : Run Model InnerModel Msg InnerMsg a
    inInner =
        runStack
            .inner
            (\m inner -> save { m | inner = inner })
            InnerMsg

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            InnerMsg innerMsg ->
                model
                    |> inInner (updateInner innerMsg)

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
