module View exposing (..)

import Models exposing (Model, StarCoordinates, Watermelon)
import Msgs exposing (Msg)
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Model -> Svg Msg
view model =
    svg [ viewBox "0 0 600 600", width "600px" ]
        [ background model.starCoordinates
        , renderWatermelons model.watermelons
        ]


background : StarCoordinates -> Svg Msg
background coordinates =
    g []
        [ rect
            [ x "0"
            , y "0"
            , width "600"
            , height "600"
            , fill "rgb(7,40,55)"
            ]
            []
        , List.map lightStar coordinates.light
            |> g []
        , List.map darkStar coordinates.dark
            |> g []
        ]


renderWatermelons : List Watermelon -> Svg Msg
renderWatermelons watermelons =
    List.map renderWatermelon watermelons
        |> g []


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
        [ cx (Tuple.first center |> toString)
        , cy (Tuple.second center |> toString)
        , r "1"
        , fill color
        ]
        []


watermelon : Float -> Float -> Float -> Float -> Svg msg
watermelon cx cy r angle =
    g []
        [ semicircle cx cy r angle "rgb(92,145,59)"
        , semicircle cx cy (r * 0.88) angle "rgb(225,232,182)"
        , semicircle cx cy (r * 0.77) angle "rgb(221,46,68)"
        ]


semicircle : Float -> Float -> Float -> Float -> String -> Svg msg
semicircle cx cy r angle color =
    let
        moveTo =
            "M" ++ toString cx ++ "," ++ toString cy

        lineTo =
            "L"
                ++ toString (cx + r * cos (angle))
                ++ ","
                ++ toString (cy + r * sin (angle))

        arcTo =
            "A"
                ++ toString r
                ++ ","
                ++ toString r
                ++ " 1 0,1 "
                ++ toString (cx + r * cos (angle + pi))
                ++ ","
                ++ toString (cy + r * sin (angle + pi))

        pathDescription =
            String.join " " [ moveTo, lineTo, arcTo, "z" ]
    in
        Svg.path [ d pathDescription, fill color ] []
