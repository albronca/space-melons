module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Random exposing (Generator, float, generate, list, map4, pair)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Posix)



-- MODEL


initialModel : Model
initialModel =
    { starCoordinates =
        { light = []
        , dark = []
        }
    , watermelons = []
    , size = { width = 600, height = 600 }
    }


type alias Model =
    { starCoordinates : StarCoordinates
    , watermelons : List Watermelon
    , size :
        { width : Int
        , height : Int
        }
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


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Task.perform
        (\{ viewport } ->
            InitialSize (round viewport.width) (round viewport.height)
        )
        Browser.Dom.getViewport
    )



-- UPDATE


type Msg
    = Tick Posix
    | GenerateStars ( List Coord, List Coord )
    | GenerateWatermelons (List Watermelon)
    | InitialSize Int Int
    | WindowResize Int Int
    | AddMelon


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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
