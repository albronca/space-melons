module View exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg)
import Svg exposing (..)
import Svg.Attributes exposing (viewBox, width, d, fill)


view : Model -> Svg Msg
view model =
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ watermelon 50 50 50 (degrees model.angle) ]


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
