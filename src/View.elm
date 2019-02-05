module View exposing (background, darkStar, lightStar, renderWatermelon, renderWatermelons, semicircle, star, view, watermelon)

import Models exposing (Model, StarCoordinates, Watermelon)
import Msgs exposing (Msg(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


view : Model -> Svg Msg
view model =
    let
        dimensions =
            String.join " "
                [ "0"
                , "0"
                , String.fromInt model.size.width
                , String.fromInt model.size.height
                ]
    in
    svg [ viewBox dimensions, width (String.fromInt model.size.width ++ "px") ]
        [ background model
        , renderWatermelons model.watermelons
        ]


background : Model -> Svg Msg
background { size, starCoordinates } =
    g []
        [ rect
            [ x "0"
            , y "0"
            , width (String.fromInt size.width)
            , height (String.fromInt size.height)
            , fill "rgb(7,40,55)"
            ]
            []
        , g [] <| List.map lightStar starCoordinates.light
        , g [] <| List.map darkStar starCoordinates.dark
        ]


renderWatermelons : List Watermelon -> Svg Msg
renderWatermelons watermelons =
    g [] <| List.map renderWatermelon watermelons


renderWatermelon : Watermelon -> Svg Msg
renderWatermelon { c, angle } =
    let
        ( cx, cy ) =
            c
    in
    watermelon cx cy 20 (degrees angle)


lightStar : ( Float, Float ) -> Svg Msg
lightStar center =
    star center "rgb(135,154,163)"


darkStar : ( Float, Float ) -> Svg Msg
darkStar center =
    star center "rgb(76,101,113)"


star : ( Float, Float ) -> String -> Svg Msg
star center color =
    circle
        [ cx (Tuple.first center |> String.fromFloat)
        , cy (Tuple.second center |> String.fromFloat)
        , r "1"
        , fill color
        ]
        []


watermelon : Float -> Float -> Float -> Float -> Svg Msg
watermelon cx cy r angle =
    g []
        [ semicircle cx cy r angle "rgb(92,145,59)"
        , semicircle cx cy (r * 0.88) angle "rgb(225,232,182)"
        , semicircle cx cy (r * 0.77) angle "rgb(221,46,68)"
        ]


semicircle : Float -> Float -> Float -> Float -> String -> Svg Msg
semicircle cx cy r angle color =
    let
        moveTo =
            "M" ++ String.fromFloat cx ++ "," ++ String.fromFloat cy

        lineTo =
            "L"
                ++ String.fromFloat (cx + r * cos angle)
                ++ ","
                ++ String.fromFloat (cy + r * sin angle)

        arcTo =
            "A"
                ++ String.fromFloat r
                ++ ","
                ++ String.fromFloat r
                ++ " 1 0,1 "
                ++ String.fromFloat (cx + r * cos (angle + pi))
                ++ ","
                ++ String.fromFloat (cy + r * sin (angle + pi))

        pathDescription =
            String.join " " [ moveTo, lineTo, arcTo, "z" ]
    in
    Svg.path [ d pathDescription, fill color ] []
