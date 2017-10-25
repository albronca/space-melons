module Models exposing (..)

import Window exposing (Size)


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
    , size : Size
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
