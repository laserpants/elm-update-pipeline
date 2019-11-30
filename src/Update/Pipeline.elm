module Update.Pipeline exposing (..)

type alias Update a msg =
    ( a, Cmd msg )


pure : a -> ( a, Cmd msg )
pure model =
    ( model, Cmd.none )


map : (a -> b) -> Update a msg -> Update b msg
map =
    Debug.todo ""


ap =
    Debug.todo ""


map2 =
    Debug.todo ""


map3 =
    Debug.todo ""


map4 =
    Debug.todo ""


map5 =
    Debug.todo ""


map6 =
    Debug.todo ""


map7 =
    Debug.todo ""


andMap =
    Debug.todo ""


join =
    Debug.todo ""


andThen next =
    join << map next


kleisli f g =
    andThen f << g


sequence list model =
    List.foldl andThen (pure model) list


filter =
    Debug.todo ""


withModel =
    Debug.todo ""


addCmd cmd model =
    ( model, cmd )


mapCmd fun ( model, cmd ) =
    ( model, Cmd.map fun cmd )


andAddCmd =
    andThen << addCmd


with : (a -> b) -> (b -> a -> c) -> a -> c
with get fun =
    using (fun << get)


using : (a -> a -> b) -> a -> b
using fun model =
    fun model model


when : Bool -> (a -> Update a msg) -> a -> Update a msg
when cond fun =
    if cond then
        fun

    else
        pure


andWith get =
    andThen << with get


andUsing =
    andThen << using


andIf cond =
    andThen << when cond

