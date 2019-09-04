module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 exposing (vec2, Vec2, normalize, scale, add)
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as H
import Html.Events exposing (onClick)
import Task
import Time
import Platform.Sub as Sub
import Debug


-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { t0 : Maybe Time.Posix
    , relativeTime : Float
    , v0 : Vec2
    , x0 : Vec2
    , acc : Vec2
    , paused : Bool
    }


accFromV : Vec2 -> Vec2
accFromV =
    normalize >> scale -0.0001


init : () -> ( Model, Cmd Msg )
init _ =
    let
        v0 =
            vec2 0.1 0.2

        x0 =
            vec2 15 15

        acc =
            accFromV v0
    in
        ( Model Nothing 0 v0 x0 acc False
        , Task.perform SetTime Time.now
        )



-- UPDATE


type Msg
    = Tick Time.Posix
    | SetTime Time.Posix
    | Pause Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                newModel =
                    model.t0
                        |> Maybe.map (\t0 -> { model | relativeTime = ((Time.posixToMillis newTime) - (Time.posixToMillis t0)) |> toFloat })
                        |> Maybe.withDefault model
            in
                ( newModel
                , Cmd.none
                )

        SetTime t ->
            ( { model | t0 = Just t }
            , Cmd.none
            )

        Pause paused ->
            ( { model | paused = not (Debug.log "Pause" paused) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then Sub.none else Browser.Events.onAnimationFrame Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        duration =
            Math.Vector2.length model.v0 / Math.Vector2.length model.acc

        endPos =
            model.x0 |> add (model.v0 |> scale duration) |> add (model.acc |> scale ((duration ^ 2) * 0.5))

        xString =
            model.x0 |> Math.Vector2.getX |> round |> String.fromInt

        yString =
            model.x0 |> Math.Vector2.getX |> round |> String.fromInt

        endPosXString =
            endPos |> Math.Vector2.getX |> round |> String.fromInt

        endPosYString =
            endPos |> Math.Vector2.getY |> round |> String.fromInt

        maxRange =
            ceiling duration

        relTimeString =
            Basics.min model.relativeTime (maxRange |> toFloat) |> String.fromFloat

        endVOverStartV =
            0

        ksx1 =
            0.333

        ksx2 =
            0.667

        ksy1 =
            2.0 / (3.0 * (1.0 + endVOverStartV))

        ksy2 =
            ksy1 + 0.333
    in
        div []
            [ input [ H.type_ "range", H.min "0", H.max (duration |> ceiling |> String.fromInt), H.value relTimeString, H.readonly model.paused ] []
            , button [ onClick (Pause model.paused), H.readonly False ]
                [ Html.text
                    (if model.paused then
                        "▶"
                     else
                        "⏸"
                    )
                ]
            , svg [ Svg.Attributes.style "position:fixed; top: 50px; left:0; height:100%; width:100%" ]
                [ Svg.circle
                    [ cx xString
                    , cy yString
                    , r "10"
                    , fill "red"
                    ]
                    [ Svg.animateMotion
                        [ Svg.Attributes.path <| String.join " " [ "M", xString, yString, "L", endPosXString, endPosYString ]
                        , keyPoints "0;1"
                        , keyTimes "0;1"
                        , keySplines ([ ksx1, ksy1, ksx2, ksy2 ] |> List.map String.fromFloat |> String.join " ")
                        , calcMode "spline"
                        , fill "freeze"
                        , dur <| String.concat [ duration |> round |> String.fromInt, "ms" ]
                        ]
                        []
                    ]
                ]
            ]
