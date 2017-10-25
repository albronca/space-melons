module Generators exposing (..)

import Models exposing (Watermelon)
import Random exposing (..)
import Window exposing (Size)


coordinateGenerator : Size -> Generator ( List ( Float, Float ), List ( Float, Float ) )
coordinateGenerator { width, height } =
    let
        listGenerator =
            pair (float 0 (toFloat width)) (float 0 (toFloat height))
                |> list 50
    in
        pair listGenerator listGenerator


watermelonGenerator : Size -> Generator (List Watermelon)
watermelonGenerator { width, height } =
    map4 Watermelon
        (pair (float 0 (toFloat width)) (float 0 (toFloat height)))
        (pair (float -3 3) (float -3 3))
        (float 0 360)
        (float -3 3)
        |> list 10
