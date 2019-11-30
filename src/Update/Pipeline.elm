module Update.Pipeline exposing
    ( save, map, addCmd, mapCmd, join
    , andThen, sequence, when, andIf, kleisli
    , map2, map3, map4, map5, map6, map7, andMap
    , using, with
    , andAddCmd, andUsing, andWith
    )

{-| This module provides an interface for sequential composition of updates in the style of pipelines,
where _monadic_ functions of the type `a -> ( b, Cmd msg )` are chained together using
the pipes operator. For example;

    update msg model =
        case msg of
            SomeMsg someMsg ->
                save model
                    |> andThen ??
                    |> andThen (setAllDone True)
                    |> andAddCmd someCmd

It also lends itself naturally to, so called, _pointfree_ style of code:

    update msg =
        case msg of
            SomeMsg someMsg ->
                save
                    >> andThen ??
                    >> andThen (setAllDone True)
                    >> andAddCmd someCmd


# Basics

@docs save, map, addCmd, mapCmd, join


# Chaining Updates

@docs andThen, sequence, when, andIf, kleisli


# Applicative Interface

These functions address the need to map over functions with more than one parameter.

@docs map2, map3, map4, map5, map6, map7, andMap


# Pointfree Helpers

@docs using, with


# Other Shortcuts

@docs andAddCmd, andUsing, andWith

-}


{-| Inject a value into a `( model, Cmd msg )` pair without adding any commands.

    save model =
        ( model, Cmd.none )

-}
save : a -> ( a, Cmd msg )
save model =
    ( model, Cmd.none )


{-| Apply a function to the model (i.e. first component) of a `( model, Cmd msg )` pair.
When partially applied, we can also think of this as taking a function `a -> b` and “lifting” it into one of type `( a, Cmd msg ) -> ( b, Cmd msg )`.
-}
map : (a -> b) -> ( a, Cmd msg ) -> ( b, Cmd msg )
map =
    Tuple.mapFirst


ap :
    ( a -> b, Cmd msg )
    -> ( a, Cmd msg )
    -> ( b, Cmd msg )
ap ( fun, cmd1 ) ( model, cmd2 ) =
    ( fun model
    , Cmd.batch [ cmd1, cmd2 ]
    )


{-| Combine two `( model, Cmd msg )` pairs by applying a function of two arguments
to their respective models.
-}
map2 :
    (p -> q -> r)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
map2 f =
    ap << map f


{-| Combine three `( model, Cmd msg )` pairs by applying a function of three arguments
to their respective models.
-}
map3 :
    (p -> q -> r -> s)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
    -> ( s, Cmd msg )
map3 f a =
    ap << map2 f a


{-| Combine four `( model, Cmd msg )` pairs by applying a function of four arguments
to their respective models.
-}
map4 :
    (p -> q -> r -> s -> t)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
    -> ( s, Cmd msg )
    -> ( t, Cmd msg )
map4 f a b =
    ap << map3 f a b


{-| Combine five `( model, Cmd msg )` pairs by applying a function of five arguments
to their respective models.
-}
map5 :
    (p -> q -> r -> s -> t -> u)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
    -> ( s, Cmd msg )
    -> ( t, Cmd msg )
    -> ( u, Cmd msg )
map5 f a b c =
    ap << map4 f a b c


{-| Combine six `( model, Cmd msg )` pairs by applying a function of six arguments
to their respective models.
-}
map6 :
    (p -> q -> r -> s -> t -> u -> v)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
    -> ( s, Cmd msg )
    -> ( t, Cmd msg )
    -> ( u, Cmd msg )
    -> ( v, Cmd msg )
map6 f a b c d =
    ap << map5 f a b c d


{-| Combine seven `( model, Cmd msg )` pairs by applying a function of seven arguments
to their respective models.
-}
map7 :
    (p -> q -> r -> s -> t -> u -> v -> w)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
    -> ( s, Cmd msg )
    -> ( t, Cmd msg )
    -> ( u, Cmd msg )
    -> ( v, Cmd msg )
    -> ( w, Cmd msg )
map7 f a b c d e =
    ap << map6 f a b c d e


{-| Trying to map over a function `number -> number -> number`,

    map (+) (save 4)

we end up with a result of type `( (number -> number), Cmd msg )`.

    type alias Model =
        { xxx : a
        , yyy : a
        , zzz : a
        }

    init : Flags -> ( Model, Cmd Msg )
    init flags =
        save Model
            |> andMap xxx
            |> andMap yyy
            |> andMap zzz

See also [`map2`](#map2), [`map3`](#map3), etc. If not sooner, you’ll need this function when you want to `mapN` for _N > 7_.

-}
andMap : ( a, Cmd msg ) -> ( a -> b, Cmd msg ) -> ( b, Cmd msg )
andMap a b =
    ap b a


{-| `andThen` is defined as `\f -> join << map f`.
-}
join : ( ( a, Cmd msg ), Cmd msg ) -> ( a, Cmd msg )
join ( ( model, cmd1 ), cmd2 ) =
    ( model
    , Cmd.batch [ cmd1, cmd2 ]
    )


{-| -}
andThen : (b -> ( a, Cmd msg )) -> ( b, Cmd msg ) -> ( a, Cmd msg )
andThen f =
    join << map f


{-| -}
kleisli :
    (b -> ( c, Cmd msg ))
    -> (a -> ( b, Cmd msg ))
    -> a
    -> ( c, Cmd msg )
kleisli f g =
    andThen f << g


{-| -}
sequence : List (a -> ( a, Cmd msg )) -> a -> ( a, Cmd msg )
sequence list model =
    List.foldl andThen (save model) list


{-| Create a `( model, Cmd msg)` pair from the two arguments.

This function is simply defined as `addCmd cmd model = ( model, cmd )`, but it can still be pretty useful.
For example, in idiomatic use, one could write

    { model | power = 100 }
        |> addCmd someCmd
        |> andThen (setStatus Done)

… instead of

    ( { model | power = 100 }, someCmd )
        |> andThen (setStatus Done)

See also [`andAddCmd`](#andAddCmd).

-}
addCmd : Cmd msg -> a -> ( a, Cmd msg )
addCmd cmd model =
    ( model, cmd )


{-| Transform the message produced by `Cmd` part of a `( model, Cmd msg )` pair.
-}
mapCmd : (msg1 -> msg2) -> ( a, Cmd msg1 ) -> ( a, Cmd msg2 )
mapCmd =
    Tuple.mapSecond << Cmd.map


{-| Shortcut for `andThen << addCmd`

Example use:

    model
        |> save
        |> andAddCmd someCmd

-}
andAddCmd : Cmd msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
andAddCmd =
    andThen << addCmd


{-| See also [`andWith`](#andWith).
-}
with : (a -> b) -> (b -> a -> c) -> a -> c
with view fun =
    using (fun << view)


{-| See also [`andUsing`](#andUsing).
-}
using : (a -> a -> b) -> a -> b
using fun model =
    fun model model


{-| Run an update if the given condition is `True`, otherwise do nothing.

See also [`andIf`](#andIf).

-}
when :
    Bool
    -> (a -> ( a, Cmd msg ))
    -> a
    -> ( a, Cmd msg )
when cond fun =
    if cond then
        fun

    else
        save


{-| Shortcut for `\view -> andThen << with view`
-}
andWith :
    (b -> c)
    -> (c -> b -> ( a, Cmd msg ))
    -> ( b, Cmd msg )
    -> ( a, Cmd msg )
andWith view =
    andThen << with view


{-| Shortcut for `andThen << using`
-}
andUsing :
    (b -> b -> ( a, Cmd msg ))
    -> ( b, Cmd msg )
    -> ( a, Cmd msg )
andUsing =
    andThen << using


{-| Shortcut for `\cond -> andThen << when cond`
-}
andIf :
    Bool
    -> (a -> ( a, Cmd msg ))
    -> ( a, Cmd msg )
    -> ( a, Cmd msg )
andIf cond =
    andThen << when cond
