module Models exposing (..)


initialModel : Model
initialModel =
    { starCoordinates =
        { light = []
        , dark = []
        }
    , watermelons = []
    }


type alias Model =
    { starCoordinates : StarCoordinates
    , watermelons : List Watermelon
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
