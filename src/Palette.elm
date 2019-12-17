module Palette exposing (..)

import Random
import Rgb


type alias Palette =
    { background : Rgb.Rgb
    , colors : List Rgb.Rgb
    }


init : Rgb.Rgb -> List Rgb.Rgb -> Palette
init background colors =
    { background = background
    , colors = colors
    }


generate : Palette -> Random.Generator Rgb.Rgb
generate p =
    Random.map (\x -> pickColorWithDefault (Rgb.invert p.background) x p.colors) (Random.int 0 <| List.length p.colors)


pickColorWithDefault : Rgb.Rgb -> Int -> List Rgb.Rgb -> Rgb.Rgb
pickColorWithDefault default idx colors =
    case pickColor idx colors of
        Nothing ->
            default

        Just color ->
            color


pickColor : Int -> List Rgb.Rgb -> Maybe Rgb.Rgb
pickColor idx list =
    case ( idx, list ) of
        ( _, [] ) ->
            Nothing

        ( 0, color :: _ ) ->
            Just color

        ( _, _ :: colors ) ->
            pickColor (idx - 1) colors
