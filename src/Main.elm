module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Json.Decode as D
import Random exposing (Generator)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)
import Task
import Time exposing (Posix)



-- MODEL


type alias Model =
    { stars : List Star
    , watermelons : List Watermelon
    , windowSize : WindowSize
    }


type alias Star =
    { center : Coord
    , color : StarColor
    }


type StarColor
    = Light
    | Dark


type alias Coord =
    ( Int, Int )


type alias Watermelon =
    { center : Coord
    , velocity : Coord
    , angle : Float
    , spin : Float
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


coordinateGenerator : WindowSize -> Generator ( Int, Int )
coordinateGenerator { width, height } =
    Random.pair (Random.int 0 width) (Random.int 0 height)


starListGenerator : Int -> WindowSize -> Generator (List Star)
starListGenerator numStars windowSize =
    Random.map2 Star
        (coordinateGenerator windowSize)
        (Random.uniform Light [ Dark ])
        |> Random.list numStars


watermelonGenerator : WindowSize -> Generator Watermelon
watermelonGenerator windowSize =
    Random.map4 Watermelon
        (coordinateGenerator windowSize)
        (Random.pair (Random.int -3 3) (Random.int -3 3))
        (Random.float 0 360)
        (Random.float -3 3)


watermelonListGenerator : Int -> WindowSize -> Generator (List Watermelon)
watermelonListGenerator numMelons windowSize =
    watermelonGenerator windowSize |> Random.list numMelons


initialModel : WindowSize -> Model
initialModel windowSize =
    { stars = []
    , watermelons = []
    , windowSize = windowSize
    }


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( initialModel windowSize
    , Cmd.batch
        [ Random.generate GenerateStars <| starListGenerator 50 windowSize
        , Random.generate GenerateWatermelons <| watermelonListGenerator 10 windowSize
        ]
    )



-- UPDATE


type Msg
    = AddWatermelon Watermelon
    | Tick Posix
    | Click Int
    | GenerateStars (List Star)
    | GenerateWatermelons (List Watermelon)
    | WindowResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddWatermelon watermelon ->
            ( { model | watermelons = watermelon :: model.watermelons }
            , Cmd.none
            )

        Click int ->
            ( model
            , Random.generate AddWatermelon <| watermelonGenerator model.windowSize
            )

        Tick _ ->
            let
                watermelons =
                    List.map (moveWatermelon model.windowSize) model.watermelons
            in
            ( { model | watermelons = watermelons }
            , Cmd.none
            )

        GenerateStars stars ->
            ( { model | stars = stars }, Cmd.none )

        GenerateWatermelons watermelons ->
            ( { model | watermelons = watermelons }, Cmd.none )

        WindowResize width height ->
            ( { model | windowSize = { width = width, height = height } }
            , Cmd.none
            )


moveWatermelon : WindowSize -> Watermelon -> Watermelon
moveWatermelon { width, height } watermelon =
    let
        { center, velocity, angle, spin } =
            watermelon

        ( cx, cy ) =
            center

        ( vx, vy ) =
            velocity

        newCx =
            wrap width cx vx

        newCy =
            wrap height cy vy

        newAngle =
            angle + spin
    in
    { watermelon | center = ( newCx, newCy ), angle = newAngle }


wrap : Int -> Int -> Int -> Int
wrap bound point velocity =
    if point + velocity >= bound + 20 then
        -20 + velocity

    else if point + velocity <= -20 then
        bound + 20 + velocity

    else
        point + velocity



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrame Tick
        , Browser.Events.onResize WindowResize
        , Browser.Events.onClick clickDecoder
        ]


clickDecoder : D.Decoder Msg
clickDecoder =
    D.map Click
        (D.field "clientX" D.int)



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
renderbackground { windowSize, stars } =
    g []
        [ rect
            [ x "0"
            , y "0"
            , width (String.fromInt windowSize.width)
            , height (String.fromInt windowSize.height)
            , fill "rgb(7,40,55)"
            ]
            []
        , g [] <| List.map renderStar stars
        ]


renderWatermelons : List Watermelon -> Svg Msg
renderWatermelons watermelons =
    g [] <| List.map renderWatermelon watermelons


renderWatermelon : Watermelon -> Svg Msg
renderWatermelon { center, angle } =
    let
        ( cx, cy ) =
            center

        r =
            20

        a =
            degrees angle
    in
    g []
        [ semicircle cx cy r a "rgb(92,145,59)"
        , semicircle cx cy (round (r * 0.88)) a "rgb(225,232,182)"
        , semicircle cx cy (round (r * 0.77)) a "rgb(221,46,68)"
        ]


renderStar : Star -> Svg Msg
renderStar star =
    let
        color =
            case star.color of
                Light ->
                    "rgb(135,154,163)"

                Dark ->
                    "rgb(76,101,113)"
    in
    circle
        [ cx (Tuple.first star.center |> String.fromInt)
        , cy (Tuple.second star.center |> String.fromInt)
        , r "1"
        , fill color
        ]
        []


semicircle : Int -> Int -> Int -> Float -> String -> Svg Msg
semicircle cx cy r angle color =
    let
        moveTo =
            "M" ++ String.fromInt cx ++ "," ++ String.fromInt cy

        lineTo =
            "L"
                ++ String.fromFloat (toFloat cx + toFloat r * cos angle)
                ++ ","
                ++ String.fromFloat (toFloat cy + toFloat r * sin angle)

        arcTo =
            "A"
                ++ String.fromInt r
                ++ ","
                ++ String.fromInt r
                ++ " 1 0,1 "
                ++ String.fromFloat (toFloat cx + toFloat r * cos (angle + pi))
                ++ ","
                ++ String.fromFloat (toFloat cy + toFloat r * sin (angle + pi))

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
