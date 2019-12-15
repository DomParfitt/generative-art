module Rgb exposing (..)


type alias Rgb =
    { r : Int
    , g : Int
    , b : Int
    }


rgb : Int -> Int -> Int -> Rgb
rgb r g b =
    { r = r, g = g, b = b }


init : Rgb
init =
    rgb 0 0 0


invert : Rgb -> Rgb
invert color =
    { r = 255 - color.r, g = 255 - color.g, b = 255 - color.b }


toString : Rgb -> String
toString { r, g, b } =
    String.join "," [ String.fromInt r, String.fromInt g, String.fromInt b ]


toSvgString : Rgb -> String
toSvgString colour =
    String.join "" [ "rgb(", toString colour, ")" ]
