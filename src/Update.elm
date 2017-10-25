module Update exposing (..)

import Generators exposing (..)
import Models exposing (Model, Watermelon)
import Msgs exposing (Msg(..))
import Random exposing (generate)
import Window exposing (Size)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                watermelons =
                    List.map (moveWatermelon model.size) model.watermelons
            in
                { model | watermelons = watermelons } ! []

        GenerateStars ( light, dark ) ->
            { model | starCoordinates = { light = light, dark = dark } } ! []

        GenerateWatermelons watermelons ->
            { model | watermelons = watermelons } ! []

        InitialSize size ->
            { model | size = size }
                ! [ generate GenerateStars <| coordinateGenerator size
                  , generate GenerateWatermelons <| watermelonGenerator size
                  ]

        WindowResize size ->
            { model | size = size } ! []


moveWatermelon : Size -> Watermelon -> Watermelon
moveWatermelon { width, height } watermelon =
    let
        { c, v, angle, spin } =
            watermelon

        ( cx, cy ) =
            c

        ( vx, vy ) =
            v

        newCx =
            wrap width cx vx

        newCy =
            wrap height cy vy

        newAngle =
            angle + spin
    in
        { watermelon | c = ( newCx, newCy ), angle = newAngle }


wrap : Int -> Float -> Float -> Float
wrap bound point velocity =
    if point + velocity >= toFloat (bound + 20) then
        -20 + velocity
    else if point + velocity <= -20 then
        toFloat (bound + 20) + velocity
    else
        point + velocity
