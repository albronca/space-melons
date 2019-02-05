module Msgs exposing (Msg(..))

import Models exposing (Coord, Watermelon)
import Time exposing (Posix)


type Msg
    = Tick Posix
    | GenerateStars ( List Coord, List Coord )
    | GenerateWatermelons (List Watermelon)
    | InitialSize Int Int
    | WindowResize Int Int
    | AddMelon
