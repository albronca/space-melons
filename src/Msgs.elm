module Msgs exposing (..)

import Models exposing (Coord, Watermelon)
import Time exposing (Time)


type Msg
    = Tick Time
    | GenerateStars ( List Coord, List Coord )
    | GenerateWatermelons (List Watermelon)
