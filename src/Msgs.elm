module Msgs exposing (..)

import Time exposing (Time)


type Msg
    = NoOp
    | Rotate Time
    | GenerateStars ( List ( Float, Float ), List ( Float, Float ) )
