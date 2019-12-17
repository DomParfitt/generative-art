module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Palette
import Particle exposing (Particle)
import Random
import Rgb
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)


getSvgViewbox : Int -> Int -> String
getSvgViewbox width height =
    String.join " " [ "0", "0", String.fromInt width, String.fromInt height ]


type alias Model =
    { width : Int
    , height : Int
    , palette : Palette.Palette
    , particles : List Particle
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { width = 1350
      , height = 600
      , particles = []
      , palette =
            { background = Rgb.rgb 0 0 0
            , colors =
                [ Rgb.rgb 32 0 40
                , Rgb.rgb 82 15 125
                , Rgb.rgb 99 53 126
                , Rgb.rgb 102 10 150
                , Rgb.rgb 132 26 200
                , Rgb.rgb 165 32 250
                , Rgb.rgb 196 106 251
                ]
            }
      }
    , Cmd.none
    )


type Msg
    = GenerateParticle
    | GenerateParticles Int
    | NewParticle Particle
    | UpdateParticles
    | NewVelocity ( Int, Int )


generateParticle : Model -> Cmd Msg
generateParticle model =
    Random.generate NewParticle
        (Particle.generate (List.length model.particles) model.width model.height model.palette)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateParticle ->
            ( model
            , generateParticle model
            )

        GenerateParticles count ->
            ( model
            , Cmd.batch <| List.repeat count <| generateParticle model
            )

        NewParticle p ->
            ( { model | particles = model.particles ++ [ p ] }, Cmd.none )

        UpdateParticles ->
            ( model, Random.generate NewVelocity <| Random.pair (Random.int -1 1) (Random.int -1 1) )

        NewVelocity ( dvx, dvy ) ->
            ( { model | particles = List.map (Particle.update model.width model.height dvx dvy) model.particles }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width <| String.fromInt model.width
            , height <| String.fromInt model.height
            , viewBox <| getSvgViewbox model.width model.height
            ]
            (rect
                [ width "100%"
                , height "100%"
                , fill <| Rgb.toSvgString model.palette.background
                ]
                []
                :: List.map Particle.render model.particles
            )
        , button [ onClick <| GenerateParticles 2000 ] [ text "generate" ]
        , button [ onClick UpdateParticles ] [ text "update" ]
        ]


subscriptions model =
    onAnimationFrameDelta (\_ -> UpdateParticles)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }
