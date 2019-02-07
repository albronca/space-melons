module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Random exposing (Generator, float, generate, list, map4, pair)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)
import Task
import Time exposing (Posix)



-- MODEL


type alias Model =
    { starCoordinates : StarCoordinates
    , watermelons : List Watermelon
    , windowSize : WindowSize
    }


type alias StarCoordinates =
    { light : List Coord
    , dark : List Coord
    }


type alias Coord =
    ( Float, Float )


type alias Watermelon =
    { c : Coord
    , v : Coord
    , angle : Float
    , spin : Float
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


initialModel : WindowSize -> Model
initialModel windowSize =
    { starCoordinates =
        { light = []
        , dark = []
        }
    , watermelons = []
    , windowSize = windowSize
    }


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( initialModel windowSize
    , Cmd.batch
        [ generate GenerateStars <| coordinateGenerator windowSize
        , generate GenerateWatermelons <| watermelonGenerator 10 windowSize
        ]
    )



-- UPDATE


type Msg
    = Tick Posix
    | GenerateStars ( List Coord, List Coord )
    | GenerateWatermelons (List Watermelon)
    | WindowResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                watermelons =
                    List.map (moveWatermelon model.windowSize) model.watermelons
            in
            ( { model | watermelons = watermelons }
            , Cmd.none
            )

        GenerateStars ( light, dark ) ->
            ( { model | starCoordinates = { light = light, dark = dark } }
            , Cmd.none
            )

        GenerateWatermelons watermelons ->
            ( { model | watermelons = watermelons }
            , Cmd.none
            )

        WindowResize width height ->
            ( { model | windowSize = { width = width, height = height } }
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


coordinateGenerator : { width : Int, height : Int } -> Generator ( List ( Float, Float ), List ( Float, Float ) )
coordinateGenerator { width, height } =
    let
        listGenerator =
            pair (float 0 (toFloat width)) (float 0 (toFloat height))
                |> list 50
    in
    pair listGenerator listGenerator


watermelonGenerator : Int -> { width : Int, height : Int } -> Generator (List Watermelon)
watermelonGenerator numMelons { width, height } =
    map4 Watermelon
        (pair (float 0 (toFloat width)) (float 0 (toFloat height)))
        (pair (float -3 3) (float -3 3))
        (float 0 360)
        (float -3 3)
        |> list numMelons



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrame Tick
        , Browser.Events.onResize WindowResize
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        dimensions =
            String.join " "
                [ "0"
                , "0"
                , String.fromInt model.windowSize.width
                , String.fromInt model.windowSize.height
                ]
    in
    { title = "space melons"
    , body =
        [ node "style" [] [ text "body { margin: 0; }" ]
        , svg
            [ viewBox dimensions
            , Svg.Attributes.style "display: block"
            ]
            [ lazy renderbackground model
            , renderWatermelons model.watermelons
            ]
        ]
    }


renderbackground : Model -> Svg Msg
renderbackground { windowSize, starCoordinates } =
    g []
        [ rect
            [ x "0"
            , y "0"
            , width (String.fromInt windowSize.width)
            , height (String.fromInt windowSize.height)
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

        r =
            20

        a =
            degrees angle
    in
    g []
        [ semicircle cx cy r a "rgb(92,145,59)"
        , semicircle cx cy (r * 0.88) a "rgb(225,232,182)"
        , semicircle cx cy (r * 0.77) a "rgb(221,46,68)"
        ]


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


main : Program WindowSize Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
