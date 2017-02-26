module Update exposing (..)

import Models exposing (Model, Watermelon)
import Msgs exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                watermelons =
                    List.map moveWatermelon model.watermelons
            in
                { model | watermelons = watermelons } ! []

        GenerateStars ( light, dark ) ->
            { model | starCoordinates = { light = light, dark = dark } } ! []

        GenerateWatermelons watermelons ->
            { model | watermelons = watermelons } ! []


moveWatermelon : Watermelon -> Watermelon
moveWatermelon watermelon =
    let
        { c, v, angle, spin } =
            watermelon

        ( cx, cy ) =
            c

        ( vx, vy ) =
            v

        newCx =
            wrap cx vx

        newCy =
            wrap cy vy

        newAngle =
            angle + spin
    in
        { watermelon | c = ( newCx, newCy ), angle = newAngle }


wrap point velocity =
    if point + velocity >= 620 then
        -20 + velocity
    else if point + velocity <= -20 then
        620 + velocity
    else
        point + velocity
