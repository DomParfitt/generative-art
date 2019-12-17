module Point exposing (..)

import Random


type alias Point =
    ( Int, Int )


toString : Point -> String
toString ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y


generate : Int -> Int -> Random.Generator Point
generate a b =
    Random.pair (Random.int 0 a) (Random.int 0 b)
