module Main exposing (..)

import Html exposing (program)
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Time exposing (every, millisecond)
import Update exposing (update)
import View exposing (view)


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init =
    { angle = 0 } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    every (20 * millisecond) Rotate
