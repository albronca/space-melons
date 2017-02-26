module Models exposing (..)


type alias Model =
    { angle : Float
    , starCoordinates : StarCoordinates
    }


type alias StarCoordinates =
    { light : List ( Float, Float )
    , dark : List ( Float, Float )
    }
