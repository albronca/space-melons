module Models exposing (Coord, Model, StarCoordinates, Watermelon, initialModel)


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
