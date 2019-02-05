module Main exposing (init, main, subscriptions)

import Browser
import Browser.Dom
import Browser.Events
import Models exposing (Model, initialModel)
import Msgs exposing (Msg(..))
import Task
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Task.perform
        (\{ viewport } ->
            InitialSize (round viewport.width) (round viewport.height)
        )
        Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrame Tick
        , Browser.Events.onResize WindowResize
        ]
