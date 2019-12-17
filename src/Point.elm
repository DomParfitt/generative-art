module Point exposing (..)

import Random


type alias Point =
    ( Int, Int )


toString : Point -> String
toString ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y


toStringList : List Point -> List String
toStringList ps =
    case ps of
        [] ->
            []

        p :: rest ->
            toString p :: toStringList rest


generatePoint : Int -> Int -> Random.Generator Point
generatePoint a b =
    Random.pair (Random.int 0 a) (Random.int 0 b)
