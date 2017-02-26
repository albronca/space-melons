module Main exposing (..)

import Html exposing (program)
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Random exposing (generate, list, pair, float)
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
    let
        model =
            { angle = 0
            , starCoordinates =
                { light = []
                , dark = []
                }
            }

        listGenerator =
            list 50 <| pair (float 0 600) (float 0 600)

        coordinateGenerator =
            pair listGenerator listGenerator
    in
        ( model, generate GenerateStars coordinateGenerator )


subscriptions : Model -> Sub Msg
subscriptions model =
    every (20 * millisecond) Rotate
