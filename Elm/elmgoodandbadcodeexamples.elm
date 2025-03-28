{- Good Code -}
import Html exposing (..)
import Html.Attributes exposing (..)

main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

init =
    ( 0, Cmd.none )

type Msg =
    Increment

update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

view model =
    div []
        [ button [ onClick Increment ] [ text "Increment" ]
        , text ("Count: " ++ String.fromInt model)
        ]

subscriptions model =
    Sub.none


{- Bad Code -}
import Html exposing (..)
import Html.Attributes exposing (..)

main =
    programWithFlags
        { init = init
        , update = \msg model -> (model, Cmd.none) --always return the same model
        , view = \model -> div [] [text "Count: " ++ String.fromInt model] --no interactivity
        , subscriptions = \_ -> Sub.none
        , flags = \x -> (0, Cmd.none) -- no actual use of flags
        }

init flags = (0, Cmd.none)

