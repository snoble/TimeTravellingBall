module Main exposing (..)

import Browser
import Browser.Events
import Debug
import Html exposing (..)
import Html.Attributes as H
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Mouse
import Math.Vector2 exposing (Vec2, add, getX, getY, normalize, vec2)
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time



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
    { relativeTime : Int
    , playTime : Maybe Time.Posix
    , v0 : Vec2
    , x0 : Vec2
    , acc : Vec2
    , paused : Bool
    , line : Maybe ( Mouse.Event, Mouse.Event )
    }


accFromV : Vec2 -> Vec2
accFromV =
    normalize >> Math.Vector2.scale -0.0001


init : () -> ( Model, Cmd Msg )
init _ =
    let
        v0 =
            vec2 0.3 0.35

        x0 =
            vec2 100 100

        acc =
            accFromV v0
    in
    ( Model 0 Nothing v0 x0 acc False Nothing
    , Task.perform Play Time.now
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Play Time.Posix
    | Pause Bool
    | MouseDownEvent Mouse.Event
    | MouseMoveEvent Mouse.Event
    | MouseUpEvent Mouse.Event
    | MouseLeaveEvent Mouse.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                newModel =
                    model.playTime
                        |> Maybe.map (\t0 -> { model | relativeTime = Time.posixToMillis newTime - Time.posixToMillis t0 })
                        |> Maybe.withDefault model
            in
            ( newModel
            , Cmd.none
            )

        Play t ->
            ( { model | playTime = Just t }
            , Cmd.none
            )

        Pause paused ->
            if paused then
                ( { model | paused = not paused, playTime = Nothing }, Task.perform Play (Time.now |> Task.map (Time.posixToMillis >> (\t -> t - model.relativeTime) >> Time.millisToPosix)) )

            else
                ( { model | paused = not paused, playTime = Nothing }, Cmd.none )

        MouseDownEvent event ->
            ( if event.isPrimary then
                { model | line = Just ( event, event ) }

              else
                model
            , Cmd.none
            )

        MouseMoveEvent event ->
            ( if event.isPrimary then
                { model | line = model.line |> Maybe.map (\( s, e ) -> ( s, event )) }

              else
                model
            , Cmd.none
            )

        MouseUpEvent event ->
            ( if event.isPrimary then
                { model | line = Nothing }

              else
                model
            , Cmd.none
            )

        MouseLeaveEvent event ->
            ( if event.isPrimary then
                { model | line = Nothing }

              else
                model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Browser.Events.onAnimationFrame Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        duration =
            Math.Vector2.length model.v0 / 0.0001

        endPos =
            posAtT model.x0 duration model.v0 0.0001

        movements =
            [ { endPos = endPos, startVelocity = model.v0, duration = duration } ]

        maxRange =
            ceiling (duration / 1000.0) * 1000

        relTimeString =
            Basics.min model.relativeTime maxRange |> String.fromInt
    in
    div
        [ H.style "display" "grid"
        , H.style "grid-template-rows" "max-content max-content 1fr"
        , H.style "height" "100%"
        , H.style "width" "100%"
        ]
        [ input [ H.type_ "range", H.min "0", H.max (duration |> ceiling |> String.fromInt), H.value relTimeString, H.readonly model.paused ] []
        , button [ onClick (Pause model.paused), H.readonly False, H.style "width" "2em" ]
            [ Html.text
                (if model.paused then
                    "▶"

                 else
                    "⏸"
                )
            ]
        , svg
            [ Svg.Attributes.style "height:100%; width:100%"
            , Mouse.onDown MouseDownEvent
            , Mouse.onMove MouseMoveEvent
            , Mouse.onUp MouseUpEvent
            , Mouse.onLeave MouseLeaveEvent
            ]
            ([ svgCircle model.x0 movements (model.relativeTime |> toFloat) -0.0001 ]
                ++ (model.line
                        |> Maybe.map
                            (\( s, e ) ->
                                let
                                    ( posX1, posY1 ) =
                                        s.pointer.offsetPos

                                    ( posX2, posY2 ) =
                                        e.pointer.offsetPos
                                in
                                [ Svg.line
                                    [ x1 (posX1 |> String.fromFloat)
                                    , y1 (posY1 |> String.fromFloat)
                                    , x2 (posX2 |> String.fromFloat)
                                    , y2 (posY2 |> String.fromFloat)
                                    , stroke "black"
                                    ]
                                    []
                                ]
                            )
                        |> Maybe.withDefault []
                   )
            )
        ]


posAtT startPos t v decelCoef =
    if Math.Vector2.lengthSquared v > 0.0 then
        let
            acc =
                accFromV v
        in
        startPos
            |> Math.Vector2.add (Math.Vector2.scale t v)
            |> Math.Vector2.add (Math.Vector2.scale ((t ^ 2.0) / 2.0) acc)

    else
        startPos


svgCircle : Vec2 -> List BallMovement -> Float -> Float -> Svg Msg
svgCircle initialPos movements time decelCoef =
    let
        { t, startPos, v, done } =
            movements
                |> List.foldl
                    (\mvmt ->
                        \state ->
                            if state.done then
                                state

                            else if state.t >= mvmt.duration then
                                { t = state.t - mvmt.duration, startPos = mvmt.endPos, v = state.v, done = False }

                            else
                                { t = state.t, startPos = state.startPos, v = mvmt.startVelocity, done = True }
                    )
                    { t = time, startPos = initialPos, v = vec2 0.0 0.0, done = False }

        pos =
            posAtT startPos t v decelCoef

        xString =
            pos |> Math.Vector2.getX |> round |> String.fromInt

        yString =
            pos |> Math.Vector2.getY |> round |> String.fromInt
    in
    Svg.circle
        [ cx xString
        , cy yString
        , r "10"
        , fill "blue"
        ]
        []


type alias BallMovement =
    { endPos : Vec2
    , startVelocity : Vec2
    , duration : Float
    }
