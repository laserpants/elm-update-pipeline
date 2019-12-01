module Update.Pipeline exposing
    ( save, map, addCmd, mapCmd, join, equals
    , andThen, sequence, when, kleisli
    , map2, map3, map4, map5, map6, map7, andMap
    , using, with
    , andAddCmd, andUsing, andWith, andThenIf
    )

{-| Sequential composition of updates facilitated by the pipe operator.


# Basics

@docs save, map, addCmd, mapCmd, join, equals


# Chaining Updates

These functions enable composition of updates by chaining together functions of the type `a -> ( b, Cmd msg )`.

@docs andThen, sequence, when, kleisli


# Applicative Interface

These functions address the need to map functions with more than one parameter over `( model, Cmd msg )` inputs.

@docs map2, map3, map4, map5, map6, map7, andMap


# Pointfree Helpers


### A note about pointfree style and η-reduction

Thanks to currying, in Elm we can often omit function arguments:

    f1 x = g x      <==>  f1 = g
    f2 x = g (h x)  <==>  f2 = g << h

Making the arguments implicit in this way allows the programmer to think about the program more abstractly, and can (sometimes) lead to more readable program code.


### Pointfree:

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg =
        case msg of
            ButtonClicked ->
                setMessage "The button was clicked!"
                    >> andThen haveCoffee


### Pointful:

    update msg model =
        case msg of
            ButtonClicked ->
                model
                    |> setMessage "The button was clicked!"
                    |> andThen haveCoffee

@docs using, with


# Shortcuts

@docs andAddCmd, andUsing, andWith, andThenIf

-}


{-| Inject a value into a `( model, Cmd msg )` pair without adding any commands.

    save model =
        ( model, Cmd.none )

-}
save : a -> ( a, Cmd msg )
save model =
    ( model, Cmd.none )


{-| Apply a function to the model (i.e. first component) of a `( model, Cmd msg )` pair.
Partially applied, we can also think of this as taking a function `a -> b` and _lifting_ it into one of type `( a, Cmd msg ) -> ( b, Cmd msg )`.
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


{-| Combine two `( model, Cmd msg )` values by applying a function of two arguments
to their respective models.
-}
map2 :
    (p -> q -> r)
    -> ( p, Cmd msg )
    -> ( q, Cmd msg )
    -> ( r, Cmd msg )
map2 f =
    ap << map f


{-| Combine three `( model, Cmd msg )` values by applying a function of three arguments
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


{-| Combine four `( model, Cmd msg )` values by applying a function of four arguments
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


{-| Combine five `( model, Cmd msg )` values by applying a function of five arguments
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


{-| Combine six `( model, Cmd msg )` values by applying a function of six arguments
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


{-| Combine seven `( model, Cmd msg )` values by applying a function of seven arguments
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


{-| Trying to map a function `(+) : number -> number -> number` over two `( model, Cmd msg)` inputs; first applying it to the first value

    map (+) (save 4)

… we end up with a result of type `( (number -> number), Cmd msg )`.
To apply the function inside this value to another `( number, Cmd msg )` value, we use this function in the following way:

    map (+) (save 4) |> andMap (save 5)

In `elm repl`, we can verify that the result is what we expect:

    > Tuple.first <| (map (+) (save 4) |> andMap (save 5))
    9 : number

This pattern scales in a nice way to functions of any number of arguments.

See also [`map2`](#map2), [`map3`](#map3), etc. If not sooner, you’ll need this function when you want to `mapN` for _N > 7_.

-}
andMap : ( a, Cmd msg ) -> ( a -> b, Cmd msg ) -> ( b, Cmd msg )
andMap a b =
    ap b a


{-| Remove one level of structure that results from composing functions of the form `a -> ( b, Cmd msg )`:

    f : a -> ( b, Cmd msg )
    g : b -> ( c, Cmd mgs )

    map g << f : a -> ( ( c, Cmd msg ), Cmd msg )

It is useful to know that [`andThen`](#andThen) is defined as `\f -> join << map f`.

-}
join : ( ( a, Cmd msg ), Cmd msg ) -> ( a, Cmd msg )
join ( ( model, cmd1 ), cmd2 ) =
    ( model
    , Cmd.batch [ cmd1, cmd2 ]
    )


{-| Compare the models of two `( model, Cmd msg)` values.

Note that the presence of effects means that

    > (save 9) == (map2 (^) (save 3) (save 2))
    False : Bool

This function can be used for comparison when only the model is of interest:

    > equals (save 9) (map2 (^) (save 3) (save 2))
    True : Bool

-}
equals : ( a, Cmd msg ) -> ( a, Cmd msg ) -> Bool
equals ( a, _ ) ( b, _ ) =
    a == b


{-| When used in conjunction with the pipe operator, this combinator extracts the model from a `( model, Cmd msg )` value and passes it as input to the next function in a pipeline.
For example;

    model
        |> setPower 100
        |> andThen (setDone True)

Monadic functions of type `a -> ( b, Cmd msg )` are the building blocks of a pipeline.
For instance, the type of `setPower` in the above example is `Int -> Model -> ( Model, Cmd Msg )`.

-}
andThen : (b -> ( a, Cmd msg )) -> ( b, Cmd msg ) -> ( a, Cmd msg )
andThen f =
    join << map f


{-| Right-to-left composition of two functions that return `( model, Cmd msg )` values, passing the first component of the first return value as input to the second function.

This is analogous to ordinary function composition in the following way:

    (<<) : (b -> c) -> (a -> b) -> a -> c

    kleisli : (b -> ( c, Cmd msg )) -> (a -> ( b, Cmd msg )) -> a -> ( c, Cmd msg )

-}
kleisli :
    (b -> ( c, Cmd msg ))
    -> (a -> ( b, Cmd msg ))
    -> a
    -> ( c, Cmd msg )
kleisli f g =
    andThen f << g


{-| Take a list of `a -> ( a, Cmd msg )` functions and run them sequentially, in a left-to-right manner, with the second argument as input.
-}
sequence : List (a -> ( a, Cmd msg )) -> a -> ( a, Cmd msg )
sequence list model =
    List.foldl andThen (save model) list


{-| Create a `( model, Cmd msg)` pair from the two arguments.
The implementation of this function is not very exciting — it is simply defined as `addCmd cmd model = ( model, cmd )` — but it can still be quite useful in idiomatic code.
For example, one could write

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


{-| Transform the message produced by the command inside a `( model, Cmd msg )` pair.
-}
mapCmd : (msg1 -> msg2) -> ( a, Cmd msg1 ) -> ( a, Cmd msg2 )
mapCmd =
    Tuple.mapSecond << Cmd.map


{-| This function is a shortcut for `andThen <<`[`addCmd`](#addCmd).

Example use:

    model
        |> save
        |> andAddCmd someCmd

-}
andAddCmd : Cmd msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
andAddCmd =
    andThen << addCmd


{-| This combinator is useful for writing code in pointfree style.

For example, the following code;

    model
        |> updateSomething
        |> andThen (\newModel -> setCounterValue (newModel.counter + 1) newModel)

… can be refactored as

    model
        |> updateSomething
        |> andThen (with .counter (setCounterValue << (+) 1))

See also [`using`](#using), [`andWith`](#andWith).

-}
with : (a -> b) -> (b -> a -> c) -> a -> c
with view fun =
    using (fun << view)


{-| This combinator is useful for writing code in pointfree style.

Consider the following example:

    goToPage : Int -> Model -> ( Model, Cmd msg )
    goToPage = ...

    nextPage model =
        goToPage (model.currentPage + 1) model

Using this helper, the above code can be refactored as

    nextPage =
        using (\{ currentPage } -> goToPage (currentPage + 1))

See also [`with`](#with), [`andUsing`](#andUsing).

-}
using : (a -> a -> b) -> a -> b
using fun model =
    fun model model


{-| Run an update if the given condition is `True`, otherwise do nothing.
For example;

    model
        |> when (power > 100) (setWarning Overflow)

See also [`andThenIf`](#andThenIf).

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


{-| Shortcut for `\view -> andThen <<`[`with`](#with)`view`.
-}
andWith :
    (b -> c)
    -> (c -> b -> ( a, Cmd msg ))
    -> ( b, Cmd msg )
    -> ( a, Cmd msg )
andWith view =
    andThen << with view


{-| Shortcut for `andThen <<`[`using`](#using).
-}
andUsing :
    (b -> b -> ( a, Cmd msg ))
    -> ( b, Cmd msg )
    -> ( a, Cmd msg )
andUsing =
    andThen << using


{-| This function is a shortcut for `\cond -> andThen <<`[`when`](#when)`cond`.

    model
        |> save
        |> andThenIf (power > 100) (setWarning Overflow)

-}
andThenIf :
    Bool
    -> (a -> ( a, Cmd msg ))
    -> ( a, Cmd msg )
    -> ( a, Cmd msg )
andThenIf cond =
    andThen << when cond
