# Elm Update Pipeline

This library provides an interface for sequential composition of updates in the style of pipelines,
where _monadic_ functions of the type `a -> ( b, Cmd msg )` are chained together using
the pipes operator. 

For example;

    update msg model =
        case msg of
            SomeMsg someMsg ->
                save model
                    |> andThen ??
                    |> andThen (setAllDone True)
                    |> andAddCmd someCmd

We use `save` to 

afsdadfs

    setColor : Color -> Model -> ( Model, Cmd msg )
    setColor color model =
        save { model | color = color }

    udpate msg model =
        model
            |> setColor Orange
            |> andThen (showToast "Color changed to orange")


This library also lends itself naturally to _pointfree_ style of code:

    update msg =
        case msg of
            SomeMsg someMsg ->
                save
                    >> andThen ??
                    >> andThen (setAllDone True)
                    >> andAddCmd someCmd


