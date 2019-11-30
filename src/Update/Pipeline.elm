module Update.Pipeline exposing
    ( pure, map, addCmd, mapCmd, join
    , andThen, sequence, when, andIf, kleisli
    , map2, map3, map4, map5, map6, map7, andMap
    , using, with
    , andAddCmd, andUsing, andWith
    )

{-|


## Basics

@docs pure, map, addCmd, mapCmd, join


## Chaining Updates

@docs andThen, sequence, when, andIf, kleisli


## Applicative Interface

These functions address the need to map over functions with more than one parameter.

@docs map2, map3, map4, map5, map6, map7, andMap


## Pointfree Helpers

@docs using, with


## Other Shortcuts

@docs andAddCmd, andUsing, andWith

-}


{-| Inject a value into a `( model, Cmd msg )` pair without adding any commands.

    pure model =
        ( model, Cmd.none )
-}
pure : a -> ( a, Cmd msg )
pure model =
    ( model, Cmd.none )


{-| Apply a function to the first component of a `( model, Cmd msg )` pair.

If partially applied, we can also think of this as taking a function `a -> b` and “lifting” it into one of type `( a, Cmd msg ) -> ( b, Cmd msg )`.
-}
map : (a -> b) -> ( a, Cmd msg ) -> ( b, Cmd msg )
map fun ( model, cmd ) =
    ( fun model
    , cmd
    )


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

    map (+) (pure 4)

we end up with a result of type `( (number -> number), Cmd msg )`. 



If not sooner, you’ll need this when you want to map functions of more than seven parameters.

See also [`map2`](#map2), [`map3`](#map3), etc.
-}
andMap : ( a, Cmd msg ) -> ( a -> b, Cmd msg ) -> ( b, Cmd msg )
andMap a b =
    ap b a


{-|
-}
join : ( ( a, Cmd msg ), Cmd msg ) -> ( a, Cmd msg )
join ( ( model, cmd1 ), cmd2 ) =
    ( model
    , Cmd.batch [ cmd1, cmd2 ]
    )


{-|
-}
andThen : (b -> ( a, Cmd msg )) -> ( b, Cmd msg ) -> ( a, Cmd msg )
andThen next =
    join << map next


{-|
-}
kleisli :
    (b -> ( c, Cmd msg ))
    -> (a -> ( b, Cmd msg ))
    -> a
    -> ( c, Cmd msg )
kleisli f g =
    andThen f << g


{-|
-}
sequence : List (a -> ( a, Cmd msg )) -> a -> ( a, Cmd msg )
sequence list model =
    List.foldl andThen (pure model) list


{-| Create a `( model, Cmd msg)` pair from the two arguments. For example;

    update msg model =
        case msg of
            SomeMsg someMsg ->
                model
                    |> addCmd myCmd
                    |> andThen (setStatus Done)

See also [`andAddCmd`](#andAddCmd).
-}
addCmd : Cmd msg -> a -> ( a, Cmd msg )
addCmd cmd model =
    ( model, cmd )


{-|
-}
mapCmd : (msg1 -> msg2) -> ( a, Cmd msg1 ) -> ( a, Cmd msg2 )
mapCmd fun ( model, cmd ) =
    ( model
    , Cmd.map fun cmd
    )


{-| Shortcut for `andThen << addCmd`
-}
andAddCmd : Cmd msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
andAddCmd =
    andThen << addCmd


{-|

See also [`andWith`](#andWith).
-}
with : (a -> b) -> (b -> a -> c) -> a -> c
with view fun =
    using (fun << view)


{-|

See also [`andUsing`](#andUsing).
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
        pure


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
