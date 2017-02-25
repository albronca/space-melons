module Update exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate _ ->
            { model | angle = model.angle + 1 } ! []

        NoOp ->
            model ! []
