module Update.Pipeline.Extended exposing (..)

import Tuple exposing (mapFirst)
import Update.Pipeline exposing (andThen, mapCmd, save, sequence)


type alias Extended m a =
    ( m, List a )


extend : a -> Extended a c
extend model =
    ( model, [] )


shuffle : Extended ( a, Cmd msg ) c -> ( Extended a c, Cmd msg )
shuffle ( ( model, cmd ), calls ) =
    ( ( model, calls ), cmd )


mapE : (a -> b) -> Extended a c -> Extended b c
mapE f ( x, calls ) =
    ( f x, calls )


mapE2 : (a -> b -> c) -> Extended a d -> Extended b d -> Extended c d
mapE2 f ( x, calls1 ) ( y, calls2 ) =
    ( f x y, calls1 ++ calls2 )


mapE3 : (a -> b -> c -> d) -> Extended a e -> Extended b e -> Extended c e -> Extended d e
mapE3 f ( x, calls1 ) ( y, calls2 ) ( z, calls3 ) =
    ( f x y z, calls1 ++ calls2 ++ calls3 )


lift :
    (a -> ( b, Cmd msg ))
    -> Extended a c
    -> ( Extended b c, Cmd msg )
lift fun a =
    shuffle (mapE fun a)


lift2 :
    (a -> b -> ( c, Cmd msg ))
    -> Extended a d
    -> Extended b d
    -> ( Extended c d, Cmd msg )
lift2 fun a b =
    shuffle (mapE2 fun a b)


lift3 :
    (a -> b -> c -> ( d, Cmd msg ))
    -> Extended a e
    -> Extended b e
    -> Extended c e
    -> ( Extended d e, Cmd msg )
lift3 fun a b c =
    shuffle (mapE3 fun a b c)


andLift :
    (a -> ( b, Cmd msg ))
    -> ( Extended a c, Cmd msg )
    -> ( Extended b c, Cmd msg )
andLift =
    andThen << lift


addCalls : List c -> Extended a c -> ( Extended a c, Cmd msg )
addCalls calls1 ( model, calls ) =
    save ( model, calls1 ++ calls )


call : c -> Extended a c -> ( Extended a c, Cmd msg )
call =
    addCalls << List.singleton


andCall : c -> ( Extended a c, Cmd msg ) -> ( Extended a c, Cmd msg )
andCall =
    andThen << call


sequenceCalls : Extended a (a -> ( a, Cmd msg )) -> ( a, Cmd msg )
sequenceCalls ( model, calls ) =
    sequence calls model


type alias Stack m m1 msg msg1 a =
    Extended m1 a -> ( Extended m1 (m -> ( m, Cmd msg )), Cmd msg1 )


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


runStack :
    (a -> m1)
    -> (a -> m1 -> ( m, Cmd msg ))
    -> (msg1 -> msg)
    -> Stack m m1 msg msg1 b
    -> a
    -> ( m, Cmd msg )
runStack =
    run identity
