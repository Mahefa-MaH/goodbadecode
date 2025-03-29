{- Good Elm code -}
import Html exposing (..)
import Html.Attributes exposing (..)

main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { count = 0 }, Cmd.none )

type alias Model =
    { count : Int }

type Msg =
    Increment
    Decrement

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )
        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , text (String.fromInt model.count)
        , button [ onClick Decrement ] [ text "-" ]
        ]


{- Bad Elm code -}
import Html exposing (..)
import Html.Attributes exposing (..)

main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( {count:Int}, Cmd msg)
init _ =
    ( {count = 0}, Cmd.none)

type alias Model =
    { count : Int }

type Msg =
    Increment | Decrement

update : Msg -> {count : Int} -> ({count : Int}, Cmd Msg)
update msg model =
    case msg of
        Increment->
            ({ count = model.count + 1 }, Cmd.none)
        Decrement->
            ({ count = model.count - 1 }, Cmd.none)

subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none

view : Model -> Html msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , text (String.fromInt model.count)
        , button [ onClick Decrement ] [ text "-" ]
        ]

