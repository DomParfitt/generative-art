module Particle exposing (..)

import Palette exposing (Palette)
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
    , x : List Int
    , y : List Int
    , color : Rgb.Rgb
    }


particle : Int -> Int -> Int -> Rgb.Rgb -> Particle
particle id x y color =
    { id = id, vx = 1, vy = 1, size = 1, direction = 0, x = [ x ], y = [ y ], color = color }


render : Particle -> Svg msg
render p =
    circle
        [ cx <| String.fromInt <| Maybe.withDefault 0 <| List.head p.x
        , cy <| String.fromInt <| Maybe.withDefault 0 <| List.head p.y
        , r <| String.fromInt p.size
        , fill <| Rgb.toSvgString p.color
        ]
        []


pointsString : List Int -> List Int -> List String
pointsString xs ys =
    case ( xs, ys ) of
        ( [], [] ) ->
            []

        ( x :: xss, y :: yss ) ->
            [ String.fromInt x ++ "," ++ String.fromInt y ] ++ pointsString xss yss

        ( _, _ ) ->
            []


renderLine : Particle -> Svg msg
renderLine p =
    polyline
        [ points <| String.join " " <| pointsString p.x p.y
        , stroke <| Rgb.toSvgString p.color
        ]
        []


generatePosition : Int -> Int -> Random.Generator ( Int, Int )
generatePosition a b =
    Random.pair (Random.int 0 a) (Random.int 0 b)


generateParticle : Int -> Int -> Int -> Palette -> Random.Generator Particle
generateParticle id w h p =
    generatePosition w h |> Random.andThen (\( x, y ) -> Palette.generateColor p |> Random.map (\c -> particle id x y c))


updateByDelta : Int -> Int -> Int -> Int
updateByDelta current delta max =
    current + delta |> modBy max


update : Int -> Int -> Int -> Int -> Particle -> Particle
update w h dvx dvy p =
    { id = p.id
    , vx = velocity p.vx dvx
    , vy = velocity p.vy dvy
    , size = p.size
    , direction = p.direction
    , x = updateByDelta (Maybe.withDefault 0 <| List.head p.x) p.vx w :: p.x
    , y = updateByDelta (Maybe.withDefault 0 <| List.head p.y) p.vy h :: p.y
    , color = p.color
    }


velocity : Int -> Int -> Int
velocity current delta =
    updateByDelta current delta 2
