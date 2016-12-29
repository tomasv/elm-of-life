import Html exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Cell = (Int, Int)

type Msg = Ping

init = [(1, 1)] ! []

update msg model =
  (model, Cmd.none)

subscriptions model =
  Sub.none

view model =
  div [] [
    div [] [text (toString model)]
    div [] [text (toString model)]
    ]
