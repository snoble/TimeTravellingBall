module Main exposing (..)

import Aberth exposing (solve)
import Browser
import Browser.Events
import Complex exposing (toCartesian)
import Debug
import Html exposing (..)
import Html.Attributes as H
import Html.Events exposing (onClick, onInput)
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


type alias VelAcc =
    { v : Vec2
    , a : Vec2
    , stopTime : Float
    }


vaZero =
    VelAcc (vec2 0.0 0.0) (vec2 0.0 0.0) 0.0


type alias BallMovement =
    { endPos : Vec2
    , startVelocity : VelAcc
    , duration : Float
    }


type alias BallPosition =
    { t : Float
    , x : Vec2
    , va : VelAcc
    }


type alias Ball =
    { color : String
    , initPosition : Vec2
    , initTime : Float
    , movements : Maybe (List BallMovement)
    }


type alias BallChange =
    { idx : Int
    , position : BallPosition
    }


type alias BallCollision =
    { ball1 : Int
    , ball2 : Int
    , t : Float
    }


type alias IndexedBallPosition =
    { idx : Int
    , pos : BallPosition
    , absoluteStopTime : Float
    }


type alias Model =
    { relativeTime : Int
    , playTime : Maybe Time.Posix
    , paused : Bool
    , line : Maybe ( Mouse.Event, Mouse.Event )
    , balls : List Ball
    , duration : Float
    , collisions : List BallCollision
    }


zeroTimePosition : BallPosition -> BallPosition
zeroTimePosition pos =
    ballPosAtT pos 0.0


collisionsFromPositions : IndexedBallPosition -> IndexedBallPosition -> List BallCollision
collisionsFromPositions position1 position2 =
    if position1.absoluteStopTime < position2.pos.t || position2.absoluteStopTime < position1.pos.t then
        []

    else
        let
            ztPos1 =
                zeroTimePosition position1.pos

            ztPos2 =
                zeroTimePosition position2.pos

            x =
                Math.Vector2.sub ztPos1.x ztPos2.x

            v =
                Math.Vector2.sub ztPos1.va.v ztPos2.va.v

            acc =
                Math.Vector2.sub ztPos1.va.a ztPos2.va.a

            polynomial =
                [ Math.Vector2.lengthSquared acc / 4.0
                , Math.Vector2.dot acc v
                , Math.Vector2.lengthSquared v + Math.Vector2.dot acc x
                , Math.Vector2.dot v x * 2.0
                , Math.Vector2.lengthSquared x - 400.0
                ]

            eps =
                0.001

            sols1 =
                solve polynomial eps

            sols =
                sols1
                    |> List.filter
                        (\c ->
                            let
                                { re, im } =
                                    c |> toCartesian
                            in
                            abs im < eps && re >= 0 && re < position1.absoluteStopTime && re < position2.absoluteStopTime
                        )
                    |> List.map (\c -> (c |> toCartesian).re)
                    |> List.map (\r -> BallCollision position1.idx position2.idx r)
        in
        sols


collisionCandidates : IndexedBallPosition -> List ( IndexedBallPosition, List BallCollision ) -> List ( IndexedBallPosition, List BallCollision )
collisionCandidates position otherPositions =
    let
        newCollisions =
            otherPositions
                |> List.concatMap
                    (\( otherPosition, otherCollisions ) ->
                        collisionsFromPositions position otherPosition
                    )
    in
    ( position, newCollisions ) :: otherPositions


nextChanges : List BallPosition -> ( List BallChange, List BallCollision )
nextChanges positions =
    let
        possibleCollisions =
            positions
                |> List.indexedMap
                    (\idx ->
                        \pos ->
                            let
                                stopTime =
                                    pos.t + pos.va.stopTime
                            in
                            IndexedBallPosition idx pos stopTime
                    )
                |> List.foldl collisionCandidates []
    in
    ( [], [] )


avFromV : Vec2 -> VelAcc
avFromV v =
    if Math.Vector2.length v == 0.0 then
        vaZero

    else
        VelAcc v (accFromV v) (Math.Vector2.length v / 0.0001)


accFromV : Vec2 -> Vec2
accFromV =
    normalize >> Math.Vector2.scale -0.0001


movementFrom : Vec2 -> Vec2 -> BallMovement
movementFrom x0 v0 =
    let
        va =
            avFromV v0

        duration =
            va.stopTime
    in
    { endPos = posAtT x0 duration va
    , startVelocity = va
    , duration = duration
    }


indexedPositionFrom : Vec2 -> Vec2 -> Float -> Int -> IndexedBallPosition
indexedPositionFrom x v t idx =
    let
        va =
            avFromV v
    in
    { idx = idx
    , pos =
        { t = t
        , x = x
        , va = va
        }
    , absoluteStopTime = va.stopTime + t
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        v1 =
            vec2 0.2 0.3

        ball1 =
            { color = "red"
            , initPosition = vec2 100 100
            , initTime = 1500.0
            , movements =
                Just
                    [ movementFrom (vec2 100 100) v1
                    ]
            }

        v2 =
            vec2 0.1 -0.35

        ball2 =
            { color = "blue"
            , initPosition = vec2 100 800
            , initTime = 500.0
            , movements =
                Just
                    [ movementFrom (vec2 100 800) v2
                    ]
            }

        balls =
            [ ball1, ball2 ]

        pos1 =
            indexedPositionFrom ball1.initPosition v1 ball1.initTime 1

        pos2 =
            indexedPositionFrom ball2.initPosition v2 ball2.initTime 2

        duration =
            balls
                |> List.foldl
                    (\ball ->
                        \maxDur ->
                            ball.movements
                                |> Maybe.withDefault []
                                |> List.foldl (\mvmt -> \totalDur -> totalDur + mvmt.duration) ball.initTime
                    )
                    0.0
    in
    ( { relativeTime = 0
      , playTime = Nothing
      , paused = False
      , line = Nothing
      , balls = balls
      , duration = duration
      , collisions = Debug.log "collisions" (collisionsFromPositions pos1 pos2)
      }
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
    | ChangeRelativeTime (Maybe Int)


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
            ( { model | playTime = Just (Time.posixToMillis t - model.relativeTime |> Time.millisToPosix) }
            , Cmd.none
            )

        Pause paused ->
            if paused then
                ( { model | paused = not paused, playTime = Nothing }, Task.perform Play Time.now )

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

        ChangeRelativeTime rtw ->
            case rtw of
                Just rt ->
                    ( { model | relativeTime = Debug.log "rt" rt }
                    , case model.playTime of
                        Just _ ->
                            Task.perform Play Time.now

                        Nothing ->
                            Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )



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
            model.duration

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
        [ input
            [ H.type_ "range"
            , H.min "0"
            , H.max (maxRange |> String.fromInt)
            , H.value relTimeString
            , H.readonly model.paused
            , onInput (String.toInt >> ChangeRelativeTime)
            ]
            []
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
            ((model.balls |> List.concatMap (\ball -> svgCircle ball (model.relativeTime |> toFloat)))
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


posAtT : Vec2 -> Float -> VelAcc -> Vec2
posAtT startPos t va =
    let
        acc =
            va.a

        v =
            va.v
    in
    startPos
        |> Math.Vector2.add (Math.Vector2.scale t v)
        |> Math.Vector2.add (Math.Vector2.scale ((t ^ 2.0) / 2.0) acc)


vaAtT : VelAcc -> Float -> VelAcc
vaAtT va t =
    { va
        | v = va.v |> Math.Vector2.add (Math.Vector2.scale t va.a)
    }


ballPosAtT : BallPosition -> Float -> BallPosition
ballPosAtT ball t =
    { t = t
    , x = posAtT ball.x (t - ball.t) ball.va
    , va = vaAtT ball.va (t - ball.t)
    }


svgCircle : Ball -> Float -> List (Svg Msg)
svgCircle ball time =
    if time < ball.initTime then
        []

    else
        let
            timeAdjusted =
                time - ball.initTime

            movements =
                ball.movements |> Maybe.withDefault []

            { t, startPos, va, done } =
                movements
                    |> List.foldl
                        (\mvmt ->
                            \state ->
                                if state.done then
                                    state

                                else if state.t >= mvmt.duration then
                                    { t = state.t - mvmt.duration, startPos = mvmt.endPos, va = state.va, done = False }

                                else
                                    { t = state.t, startPos = state.startPos, va = mvmt.startVelocity, done = True }
                        )
                        { t = timeAdjusted, startPos = ball.initPosition, va = vaZero, done = False }

            pos =
                posAtT startPos t va

            xString =
                pos |> Math.Vector2.getX |> round |> String.fromInt

            yString =
                pos |> Math.Vector2.getY |> round |> String.fromInt
        in
        [ Svg.circle
            [ cx xString
            , cy yString
            , r "10"
            , fill ball.color
            ]
            []
        ]
