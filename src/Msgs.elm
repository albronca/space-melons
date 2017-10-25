module Msgs exposing (..)

import Models exposing (Coord, Watermelon)
import Time exposing (Time)
import Window exposing (Size)


type Msg
    = Tick Time
    | GenerateStars ( List Coord, List Coord )
    | GenerateWatermelons (List Watermelon)
    | InitialSize Size
    | WindowResize Size
