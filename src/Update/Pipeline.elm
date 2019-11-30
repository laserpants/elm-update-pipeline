module Update.Pipeline exposing
    ( pure, map, addCmd, mapCmd, join
    , andThen, sequence, when, andIf, kleisli
    , map2, map3, map4, map5, map6, map7, andMap, ap
    , using, with
    , andAddCmd, andUsing, andWith
    )

{-| TODO


## Basics

@docs pure, map, addCmd, mapCmd, join


## Chaining Updates

@docs andThen, sequence, when, andIf, kleisli


## Applicative Interface

These functions address the need to map over functions having more than one argument.

@docs map2, map3, map4, map5, map6, map7, andMap, ap


## Pointfree Helpers

@docs using, with


## Other Shortcuts

@docs andAddCmd, andMap, andThen, andUsing, andWith

-}


pure : a -> ( a, Cmd msg )
pure model =
    ( model, Cmd.none )


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


map2 :
    (p -> q -> r)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
map2 f =
    ap << map f


map3 :
    (p -> q -> r -> s)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
    -> ( s, Cmd msg )
map3 f a =
    ap << map2 f a


map4 :
    (p -> q -> r -> s -> t)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
    -> ( s, Cmd msg )
    -> ( t, Cmd msg )
map4 f a b =
    ap << map3 f a b


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


andMap : ( a, Cmd msg ) -> ( a -> b, Cmd msg ) -> ( b, Cmd msg )
andMap a b =
    ap b a


join : ( ( a, Cmd msg ), Cmd msg ) -> ( a, Cmd msg )
join ( ( model, cmd1 ), cmd2 ) =
    ( model
    , Cmd.batch [ cmd1, cmd2 ]
    )


andThen : (b -> ( a, Cmd msg )) -> ( b, Cmd msg ) -> ( a, Cmd msg )
andThen next =
    join << map next


kleisli :
    (b -> ( c, Cmd msg ))
    -> (a -> ( b, Cmd msg ))
    -> a
    -> ( c, Cmd msg )
kleisli f g =
    andThen f << g


sequence : List (a -> ( a, Cmd msg )) -> a -> ( a, Cmd msg )
sequence list model =
    List.foldl andThen (pure model) list


addCmd : Cmd msg -> a -> ( a, Cmd msg )
addCmd cmd model =
    ( model, cmd )


mapCmd : (msg1 -> msg2) -> ( a, Cmd msg1 ) -> ( a, Cmd msg2 )
mapCmd fun ( model, cmd ) =
    ( model
    , Cmd.map fun cmd
    )


andAddCmd : Cmd msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
andAddCmd =
    andThen << addCmd


with : (a -> b) -> (b -> a -> c) -> a -> c
with view fun =
    using (fun << view)


using : (a -> a -> b) -> a -> b
using fun model =
    fun model model


when :
    Bool
    -> (a -> ( a, Cmd msg ))
    -> a
    -> ( a, Cmd msg )
when check fun =
    if check then
        fun

    else
        pure


andWith :
    (b -> c)
    -> (c -> b -> ( a, Cmd msg ))
    -> ( b, Cmd msg )
    -> ( a, Cmd msg )
andWith view =
    andThen << with view


andUsing :
    (b -> b -> ( a, Cmd msg ))
    -> ( b, Cmd msg )
    -> ( a, Cmd msg )
andUsing =
    andThen << using


andIf :
    Bool
    -> (a -> ( a, Cmd msg ))
    -> ( a, Cmd msg )
    -> ( a, Cmd msg )
andIf check =
    andThen << when check
