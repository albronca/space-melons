module Main exposing (..)

import Html exposing (Html, program)
import Models exposing (Model, Watermelon, initialModel)
import Msgs exposing (Msg(..))
import AnimationFrame exposing (times)
import Task
import Update exposing (update)
import View exposing (view)
import Window


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform InitialSize Window.size )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.times Tick
        , Window.resizes WindowResize
        ]
