module Particle exposing (..)

import Palette exposing (Palette)
import Point exposing (Point)
import Random
import Rgb
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Particle =
    { id : Int
    , vx : Int
    , vy : Int
    , size : Int
    , direction : Int
    , points : List Point
    , color : Rgb.Rgb
    }


init : Int -> Point -> Rgb.Rgb -> Particle
init id origin color =
    { id = id, vx = 1, vy = 1, size = 1, direction = 0, points = [ origin ], color = color }


render : Particle -> Svg msg
render p =
    let
        ( x, y ) =
            Maybe.withDefault ( 0, 0 ) <| List.head p.points
    in
    circle
        [ cx <| String.fromInt <| x
        , cy <| String.fromInt <| y
        , r <| String.fromInt p.size
        , fill <| Rgb.toSvgString p.color
        ]
        []


renderLine : Particle -> Svg msg
renderLine p =
    polyline
        [ points <| String.join " " <| Point.toStringList p.points
        , stroke <| Rgb.toSvgString p.color
        ]
        []


generateParticle : Int -> Int -> Int -> Palette -> Random.Generator Particle
generateParticle id w h p =
    Point.generatePoint w h |> Random.andThen (\( x, y ) -> Palette.generateColor p |> Random.map (\c -> init id ( x, y ) c))


updateByDelta : Int -> Int -> Int -> Int
updateByDelta current delta max =
    current + delta |> modBy max


update : Int -> Int -> Int -> Int -> Particle -> Particle
update w h dvx dvy p =
    let
        ( x, y ) =
            Maybe.withDefault ( 0, 0 ) <| List.head p.points

        newX =
            updateByDelta x p.vx w

        newY =
            updateByDelta y p.vy h
    in
    { id = p.id
    , vx = velocity p.vx dvx
    , vy = velocity p.vy dvy
    , size = p.size
    , direction = p.direction
    , points = ( newX, newY ) :: p.points
    , color = p.color
    }


velocity : Int -> Int -> Int
velocity current delta =
    updateByDelta current delta 2
