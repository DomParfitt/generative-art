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
    , points : List (List Point)
    , color : Rgb.Rgb
    }


init : Int -> Point -> Rgb.Rgb -> Particle
init id origin color =
    { id = id, vx = 1, vy = 1, size = 1, direction = 0, points = [ [ origin ] ], color = color }


generate : Int -> Int -> Int -> Palette -> Random.Generator Particle
generate id w h p =
    Point.generate w h |> Random.andThen (\( x, y ) -> Palette.generate p |> Random.map (\c -> init id ( x, y ) c))


currentPosition : List (List Point) -> Point
currentPosition pss =
    case pss of
        [] ->
            ( 0, 0 )

        head :: tail ->
            case head of
                [] ->
                    currentPosition tail

                x :: _ ->
                    x



--Maybe.withDefault ( 0, 0 ) <| List.head p.points


renderPoints : List Point -> Rgb.Rgb -> Svg msg
renderPoints ps color =
    polyline
        [ points <| String.join " " <| List.map Point.toString ps
        , stroke <| Rgb.toSvgString color
        ]
        []


render : Particle -> Svg msg
render p =
    g []
        (List.map (\points -> renderPoints points p.color) p.points)


update : Int -> Int -> Int -> Int -> Particle -> Particle
update w h dvx dvy p =
    let
        ( x, y ) =
            currentPosition p.points

        newX =
            updateByDelta x p.vx w

        newY =
            updateByDelta y p.vy h

        head =
            Maybe.withDefault [] <| List.head p.points

        tail =
            Maybe.withDefault [] <| List.tail p.points

        points =
            if (x + p.vx >= w) || (y + p.vy >= h) then
                [ ( newX, newY ) ] :: (head :: tail)

            else
                (( newX, newY ) :: head) :: tail
    in
    { id = p.id
    , vx = velocity p.vx dvx
    , vy = velocity p.vy dvy
    , size = p.size
    , direction = p.direction
    , points = points
    , color = p.color
    }


velocity : Int -> Int -> Int
velocity current delta =
    updateByDelta current delta 2


updateByDelta : Int -> Int -> Int -> Int
updateByDelta current delta max =
    current + delta |> modBy max
