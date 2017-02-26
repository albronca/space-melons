module Main exposing (..)

import Html exposing (program)
import Models exposing (Model, Watermelon, initialModel)
import Msgs exposing (Msg(..))
import Random exposing (Generator, generate, list, pair, float, map4)
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
    ( initialModel
    , Cmd.batch
        [ generate GenerateStars coordinateGenerator
        , generate GenerateWatermelons watermelonGenerator
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    every (20 * millisecond) Tick


coordinateGenerator : Generator ( List ( Float, Float ), List ( Float, Float ) )
coordinateGenerator =
    let
        listGenerator =
            list 50 <| pair (float 0 600) (float 0 600)
    in
        pair listGenerator listGenerator


watermelonGenerator : Generator (List Watermelon)
watermelonGenerator =
    map4 Watermelon
        (pair (float 0 600) (float 0 600))
        (pair (float -3 3) (float -3 3))
        (float 0 360)
        (float -3 3)
        |> list 10
