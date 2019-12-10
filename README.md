# Elm Update Pipeline

[![Build Status](https://img.shields.io/travis/laserpants/elm-update-pipeline/master.svg?style=flat)](https://travis-ci.org/laserpants/elm-update-pipeline)
[![Version](https://img.shields.io/badge/elm--version-0.19-blue.svg?colorB=ff69b4)](http://elm-lang.org/)

A library for sequential composition of updates in the convenient style of _pipelines_, where functions are chained together using the pipe operator.
For example;

```elm
update msg model =
    case msg of
        SomeMsg someMsg ->
            save model
                |> andThen (setPower 100)
                |> andAddCmd someCmd
```

Monadic functions of type `a -> ( b, Cmd msg )` form the building blocks of a pipeline.
We use `save` to create an update without any commands, and `andThen` to extract the model from a result and pass it as input to the next function in the pipeline.

```elm
showToast : String -> Model -> ( Model, Cmd msg )
showToast = ...

setColor : Color -> Model -> ( Model, Cmd msg )
setColor color model =
    save { model | color = color }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model
        |> setColor Orange
        |> andThen (showToast "Color changed to orange")
```

The applicative interface, `map2`, `map3`, etc., together with `andMap`, addresses the need to map functions with more than one parameter over `( model, Cmd msg )` inputs.

```elm
type alias Model =
    { menuOpen : Bool, session : Session, router : Router.Model }

initSession : Flags -> ( Session, Cmd Msg )
initSession = ...

init : Flags -> ( Model, Cmd Msg )
init flags =
    save Model
        |> andMap (save False)
        |> andMap (initSession flags)
        |> andMap initRouter
```

In this example, `init` can also be defined as

```elm
init flags =
    map3 Model (save False) (initSession flags) initRouter
```
