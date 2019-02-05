module Update exposing (moveWatermelon, update, wrap)

import Generators exposing (..)
import Models exposing (Model, Watermelon)
import Msgs exposing (Msg(..))
import Random exposing (generate)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                watermelons =
                    List.map (moveWatermelon model.size) model.watermelons
            in
            ( { model | watermelons = watermelons }
            , Cmd.none
            )

        GenerateStars ( light, dark ) ->
            ( { model | starCoordinates = { light = light, dark = dark } }
            , Cmd.none
            )

        GenerateWatermelons watermelons ->
            ( { model | watermelons = model.watermelons ++ watermelons }
            , Cmd.none
            )

        AddMelon ->
            ( model
            , generate GenerateWatermelons <| watermelonGenerator 1 model.size
            )

        InitialSize width height ->
            let
                size =
                    { width = width, height = height }
            in
            ( { model | size = size }
            , Cmd.batch
                [ generate GenerateStars <| coordinateGenerator size
                , generate GenerateWatermelons <| watermelonGenerator 10 size
                ]
            )

        WindowResize width height ->
            ( { model | size = { width = width, height = height } }
            , Cmd.none
            )


moveWatermelon : { width : Int, height : Int } -> Watermelon -> Watermelon
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
