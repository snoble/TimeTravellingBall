module Main exposing (..)

import Aberth exposing (solve)
import Browser
import Browser.Events
import Complex exposing (toCartesian)
import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as H
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Pointer as Mouse
import List.Extra exposing (minimumBy)
import List.Nonempty as NE exposing (Nonempty)
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
    { ball1 : IndexedBallPosition
    , ball2 : IndexedBallPosition
    , t : Float
    }


type alias IndexedBallPosition =
    { idx : Int
    , pos : BallPosition
    }


type alias Model =
    { relativeTime : Int
    , playTime : Maybe Time.Posix
    , paused : Bool
    , line : Maybe ( Mouse.Event, Mouse.Event )
    , balls : List Ball
    , duration : Float
    , collisions : Maybe BallCollision
    }


zeroTimePosition : BallPosition -> BallPosition
zeroTimePosition pos =
    ballPosAtT pos 0.0


createBalls : List ( String, BallPosition ) -> List Ball
createBalls initialState =
    let
        movementData =
            initialState
                |> List.indexedMap (\i -> \( _, pos ) -> ( i, NE.fromElement pos ))
                |> Dict.fromList
                |> generatePositions
                |> Dict.values
                |> List.map movementsFromPositions
    in
    List.map2
        (\( initialPosition, t, movements ) ->
            \( color, _ ) ->
                { color = color
                , initPosition = initialPosition
                , initTime = t
                , movements = Just movements
                }
        )
        movementData
        initialState



-- assumes the last postion is the stopped ball


movementsFromPositions : Nonempty BallPosition -> ( Vec2, Float, List BallMovement )
movementsFromPositions positions =
    let
        firstP =
            positions |> NE.head

        tail =
            positions |> NE.tail

        initPosition =
            firstP.x

        initTime =
            firstP.t

        movements =
            tail
                |> List.foldl
                    (\thisP ->
                        \( previousP, movementsSoFar ) ->
                            ( thisP
                            , { endPos = thisP.x, startVelocity = previousP.va, duration = thisP.t - previousP.t } :: movementsSoFar
                            )
                    )
                    ( firstP, [] )
                |> Tuple.second
                |> List.reverse
    in
    ( initPosition, initTime, movements )


collisionsFromPositions : IndexedBallPosition -> IndexedBallPosition -> Maybe BallCollision
collisionsFromPositions position1 position2 =
    if (position1 |> absoluteStopTime) < position2.pos.t || (position2 |> absoluteStopTime) < position1.pos.t then
        Nothing

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

            minT =
                Basics.max position1.pos.t position2.pos.t

            polynomial =
                [ Math.Vector2.lengthSquared acc / 4.0
                , Math.Vector2.dot acc v
                , Math.Vector2.lengthSquared v + Math.Vector2.dot acc x
                , Math.Vector2.dot v x * 2.0
                , Math.Vector2.lengthSquared x - 400.0
                ]

            eps =
                0.000001

            sols =
                solve polynomial 0.001
        in
        sols
            |> List.filterMap
                (\c ->
                    let
                        { re, im } =
                            c |> toCartesian
                    in
                    if abs im < eps && re >= minT && re < (position1 |> absoluteStopTime) && re < (position2 |> absoluteStopTime) then
                        Just (c |> toCartesian).re

                    else
                        Nothing
                )
            |> List.sort
            |> List.foldl
                (\t ->
                    \collision ->
                        case collision of
                            Just _ ->
                                collision

                            Nothing ->
                                let
                                    candidatePosition1 =
                                        ballPosAtT position1.pos t

                                    candidatePosition2 =
                                        ballPosAtT position2.pos t

                                    xt =
                                        Math.Vector2.sub candidatePosition1.x candidatePosition2.x

                                    vt =
                                        Math.Vector2.sub candidatePosition1.va.v candidatePosition2.va.v

                                    dp =
                                        Math.Vector2.dot xt vt
                                in
                                if dp < 0 then
                                    let
                                        ( postCollision1, postCollision2 ) =
                                            positionsAfterCollision candidatePosition1 candidatePosition2
                                    in
                                    Just (BallCollision { position1 | pos = postCollision1 } { position2 | pos = postCollision2 } t)

                                else
                                    Nothing
                )
                Nothing


collisionCandidates : IndexedBallPosition -> List ( IndexedBallPosition, List BallCollision ) -> List ( IndexedBallPosition, List BallCollision )
collisionCandidates position otherPositions =
    let
        newCollisions =
            otherPositions
                |> List.filterMap
                    (\( otherPosition, otherCollisions ) ->
                        collisionsFromPositions position otherPosition
                    )
    in
    ( position, newCollisions ) :: otherPositions


orthogonal : Vec2 -> Vec2
orthogonal x =
    vec2 (x |> getY) ((x |> getX) * -1)


positionsAfterCollision : BallPosition -> BallPosition -> ( BallPosition, BallPosition )
positionsAfterCollision pos1 pos2 =
    let
        v1MinusV2 =
            Math.Vector2.sub pos1.va.v pos2.va.v

        x1MinusX2 =
            Math.Vector2.direction pos1.x pos2.x

        scale =
            Math.Vector2.dot v1MinusV2 x1MinusX2

        newV1 =
            Math.Vector2.sub pos1.va.v (Math.Vector2.scale scale x1MinusX2)

        newV2 =
            Math.Vector2.sub pos2.va.v (Math.Vector2.scale -scale x1MinusX2)

        newPos1 =
            { pos1 | va = avFromV newV1 }

        newPos2 =
            { pos2 | va = avFromV newV2 }
    in
    ( newPos1, newPos2 )


nextChanges : List BallPosition -> List IndexedBallPosition
nextChanges positions =
    let
        indexedPositions =
            positions
                |> List.indexedMap
                    (\idx ->
                        \pos ->
                            IndexedBallPosition idx pos
                    )

        possibleCollisions =
            indexedPositions
                |> List.foldl collisionCandidates []
                |> List.concatMap (\( _, cols ) -> cols)

        nextStop =
            indexedPositions
                |> List.filter (\pos -> pos.pos.va.stopTime > 0)
                |> minimumBy (\pos -> pos |> absoluteStopTime)
                |> Maybe.map
                    (\pos ->
                        { pos
                            | pos =
                                { x = posAtT pos.pos.x pos.pos.va.stopTime pos.pos.va
                                , va = vaZero
                                , t = pos.pos.t + pos.pos.va.stopTime
                                }
                        }
                    )

        nextCollision =
            possibleCollisions
                |> minimumBy (\col -> col.t)
    in
    case ( nextStop, nextCollision ) of
        ( Just ns, Nothing ) ->
            [ ns ]

        ( Nothing, Just nc ) ->
            [ nc.ball1, nc.ball2 ]

        ( Just ns, Just nc ) ->
            if ns.pos.t < nc.t then
                [ ns ]

            else
                [ nc.ball1, nc.ball2 ]

        ( Nothing, Nothing ) ->
            []


generatePositions : Dict Int (Nonempty BallPosition) -> Dict Int (Nonempty BallPosition)
generatePositions ballPositions =
    case nextChanges (ballPositions |> Dict.values |> List.map NE.head) of
        [] ->
            ballPositions |> Dict.map (\_ -> NE.reverse)

        indexedPositions ->
            generatePositions
                (indexedPositions
                    |> List.foldl
                        (\{ idx, pos } ->
                            Dict.update idx (Maybe.map (NE.cons pos))
                        )
                        ballPositions
                )


absoluteStopTime : IndexedBallPosition -> Float
absoluteStopTime ball =
    ball.pos.t + ball.pos.va.stopTime


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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        v1 =
            vec2 0.2 0.3

        v2 =
            vec2 0.1 -0.35

        balls =
            createBalls
                [ ( "red", BallPosition 1500.0 (vec2 100 100) (avFromV v1) )
                , ( "blue", BallPosition 500.0 (vec2 100 800) (avFromV v2) )
                ]

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
      , collisions = Nothing
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
