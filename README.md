# Elm Update Pipeline

This library defines an interface for sequential composition of updates in the convenient style of _pipelines_,
where functions are chained together using the pipe operator. For example;

    update msg model =
        case msg of
            SomeMsg someMsg ->
                save model
                    |> andThen (setPower 100)
                    |> andAddCmd someCmd

Monadic functions of type `a -> ( b, Cmd msg )` are the building blocks of a pipeline.
We use `save` to create an update without any commands, and `andThen` to extract the model from a result and pass it as input to the next function in the pipeline.

    showToast : String -> Model -> ( Model, Cmd msg )
    showToast =
        ...

    setColor : Color -> Model -> ( Model, Cmd msg )
    setColor color model =
        save { model | color = color }

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        model
            |> setColor Orange
            |> andThen (showToast "Color changed to orange")
