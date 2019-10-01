module Main exposing (..)

import Aberth exposing (solve)
import Browser
import Browser.Events
import Complex exposing (toCartesian)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as H
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Pointer as Mouse
import List.Extra exposing (minimumBy)
import List.Nonempty as NE exposing (Nonempty)
import Math.Matrix4
import Math.Vector2 exposing (Vec2, add, getX, getY, normalize, vec2)
import Math.Vector3 exposing (vec3)
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


rotateVelAcc : (Vec2 -> Vec2) -> VelAcc -> VelAcc
rotateVelAcc fn va =
    { va | v = fn va.v, a = fn va.a }


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


type alias Portal =
    { entrance : Vec2
    , exit : Vec2
    , rotation : Vec2 -> Vec2
    , timeDelta : Float
    }


createRotation : Float -> Vec2 -> Vec2
createRotation angle =
    let
        matrix =
            Math.Matrix4.makeRotate angle (vec3 0 0 1)
    in
    \v2 ->
        let
            v3 =
                Math.Matrix4.transform matrix (vec3 (getX v2) (getY v2) 0)
        in
        vec2 (Math.Vector3.getX v3) (Math.Vector3.getY v3)


createPortal : Vec2 -> Vec2 -> Float -> Float -> Portal
createPortal entrance exit angle timeDelta =
    { entrance = entrance
    , exit = exit
    , rotation = createRotation angle
    , timeDelta = timeDelta
    }


type alias Line =
    { s : Vec2
    , e : Vec2
    , time : Float
    , fixed : Bool
    }


type alias Model =
    { relativeTime : Int
    , playTime : Maybe Time.Posix
    , paused : Bool
    , line : Maybe Line
    , balls : List Ball
    , duration : Float
    , portal : Portal
    , initBall : ( String, BallPosition )
    , ghosts : List Ball
    }


zeroTimePosition : BallPosition -> BallPosition
zeroTimePosition pos =
    ballPosAtT pos 0.0


createBalls : List ( String, BallPosition ) -> List Portal -> List Ball
createBalls initialState portals =
    let
        movementData =
            initialState
                |> List.indexedMap (\i -> \( _, pos ) -> ( i, NE.fromElement pos ))
                |> Dict.fromList
                |> generatePositions portals
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


interceptTime : BallPosition -> BallPosition -> List Float
interceptTime position1 position2 =
    if (position1 |> absoluteStopTime) < position2.t || (position2 |> absoluteStopTime) < position1.t then
        []

    else
        let
            ztPos1 =
                zeroTimePosition position1

            ztPos2 =
                zeroTimePosition position2

            x =
                Math.Vector2.sub ztPos1.x ztPos2.x

            v =
                Math.Vector2.sub ztPos1.va.v ztPos2.va.v

            acc =
                Math.Vector2.sub ztPos1.va.a ztPos2.va.a

            minT =
                Basics.max position1.t position2.t

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


collisionsFromPositions : IndexedBallPosition -> IndexedBallPosition -> Maybe BallCollision
collisionsFromPositions position1 position2 =
    interceptTime position1.pos position2.pos
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


possiblePortalJumps : List Portal -> List IndexedBallPosition -> List ( IndexedBallPosition, Float )
possiblePortalJumps portals balls =
    portals
        |> List.concatMap
            (\portal ->
                balls
                    |> List.concatMap
                        (\ball ->
                            let
                                times =
                                    interceptTime { t = 0, x = portal.entrance, va = vaZero } ball.pos
                            in
                            times
                                |> List.map
                                    (\t ->
                                        let
                                            enteringPos =
                                                ballPosAtT ball.pos t

                                            exitingPos =
                                                { enteringPos
                                                    | x = portal.exit
                                                    , va = rotateVelAcc portal.rotation enteringPos.va
                                                }
                                        in
                                        ( { ball | pos = exitingPos }, t )
                                    )
                        )
            )


nextChanges : List BallPosition -> List Portal -> List IndexedBallPosition
nextChanges positions portals =
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
                |> minimumBy (\pos -> pos.pos |> absoluteStopTime)
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

        nextPortalJump =
            possiblePortalJumps portals indexedPositions |> minimumBy Tuple.second

        nextChange =
            [ nextStop |> Maybe.map (\ns -> ( ns.pos.t, Stop ns ))
            , nextCollision |> Maybe.map (\nc -> ( nc.t, Collision nc ))
            , nextPortalJump |> Maybe.map (\np -> ( np |> Tuple.second, PortalJump np ))
            ]
                |> List.filterMap identity
                |> minimumBy (\( t, _ ) -> t)
                |> Maybe.map Tuple.second
    in
    case nextChange of
        Nothing ->
            []

        Just (Stop ns) ->
            [ ns ]

        Just (Collision nc) ->
            [ nc.ball1, nc.ball2 ]

        Just (PortalJump ( ball, t )) ->
            []


type NextChange
    = Stop IndexedBallPosition
    | Collision BallCollision
    | PortalJump ( IndexedBallPosition, Float )


generatePositions : List Portal -> Dict Int (Nonempty BallPosition) -> Dict Int (Nonempty BallPosition)
generatePositions portals ballPositions =
    case nextChanges (ballPositions |> Dict.values |> List.map NE.head) portals of
        [] ->
            ballPositions |> Dict.map (\_ -> NE.reverse)

        indexedPositions ->
            generatePositions portals
                (indexedPositions
                    |> List.foldl
                        (\{ idx, pos } ->
                            Dict.update idx (Maybe.map (NE.cons pos))
                        )
                        ballPositions
                )


absoluteStopTime : BallPosition -> Float
absoluteStopTime pos =
    pos.t + pos.va.stopTime


accIntensity =
    0.0001


avFromV : Vec2 -> VelAcc
avFromV v =
    if Math.Vector2.length v == 0.0 then
        vaZero

    else
        VelAcc v (accFromV v) (Math.Vector2.length v / accIntensity)


accFromV : Vec2 -> Vec2
accFromV =
    normalize >> Math.Vector2.scale -accIntensity


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


durationFromBalls : List Ball -> Float
durationFromBalls balls =
    balls
        |> List.foldl
            (\ball ->
                \maxDur ->
                    ball.movements
                        |> Maybe.withDefault []
                        |> List.foldl (\mvmt -> \totalDur -> totalDur + mvmt.duration) ball.initTime
            )
            0.0


init : () -> ( Model, Cmd Msg )
init _ =
    let
        v1 =
            vec2 0.2 0.3

        initBall =
            ( "red", BallPosition 1500.0 (vec2 100 100) (avFromV v1) )

        portal =
            createPortal (vec2 500 100) (vec2 100 800) 20 5000

        balls =
            createBalls
                [ initBall
                ]
                [ portal ]

        duration =
            durationFromBalls balls
    in
    ( { relativeTime = 0
      , playTime = Nothing
      , paused = False
      , line = Nothing
      , initBall = initBall
      , balls = balls
      , duration = duration
      , portal = portal
      , ghosts = []
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


pe2Vec2 : Mouse.Event -> Vec2
pe2Vec2 event =
    let
        ( x, y ) =
            event.pointer.offsetPos
    in
    vec2 x y


vecToExitStartingPoint : Portal -> Vec2 -> Maybe Vec2
vecToExitStartingPoint portal x =
    if Math.Vector2.distance x portal.exit > 20.0 then
        Nothing

    else
        let
            direction =
                if Math.Vector2.distance x portal.exit == 0.0 then
                    vec2 1 0

                else
                    Math.Vector2.direction x portal.exit
        in
        Just (Math.Vector2.add portal.exit (Math.Vector2.scale 15.0 direction))


updateWithNewLine : Model -> Line -> Model
updateWithNewLine model line =
    let
        distance =
            Math.Vector2.distance line.e line.s

        initV =
            Math.Vector2.direction line.e line.s |> Math.Vector2.scale (sqrt (distance * accIntensity * 2.0))

        pos2 =
            { t = line.time, x = line.s, va = avFromV initV }

        balls =
            createBalls [ model.initBall, ( "blue", pos2 ) ] [ model.portal ]
    in
    { model | line = Just line, balls = balls, duration = durationFromBalls balls }


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
                let
                    v =
                        pe2Vec2 event

                    line =
                        v |> vecToExitStartingPoint model.portal |> Maybe.map (\s -> Line s v (model.relativeTime |> toFloat) False)
                in
                { model
                    | line =
                        case line of
                            Just _ ->
                                line

                            _ ->
                                model.line
                }

              else
                model
            , Cmd.none
            )

        MouseMoveEvent event ->
            ( if event.isPrimary then
                { model
                    | line =
                        model.line
                            |> Maybe.map
                                (\line ->
                                    if line.fixed then
                                        line

                                    else
                                        { line | e = pe2Vec2 event }
                                )
                }

              else
                model
            , Cmd.none
            )

        MouseUpEvent event ->
            ( if event.isPrimary then
                case model.line of
                    Just line ->
                        updateWithNewLine model { line | fixed = True }

                    Nothing ->
                        model

              else
                model
            , Cmd.none
            )

        MouseLeaveEvent event ->
            ( if event.isPrimary then
                case model.line of
                    Just line ->
                        updateWithNewLine model { line | fixed = True }

                    Nothing ->
                        model

              else
                model
            , Cmd.none
            )

        ChangeRelativeTime rtw ->
            case rtw of
                Just rt ->
                    ( { model | relativeTime = rt }
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
            (svgPortal model.portal
                ++ (model.balls |> List.concatMap (\ball -> svgBall ball (model.relativeTime |> toFloat)))
                ++ svgLine model.line
            )
        ]


svgLine : Maybe Line -> List (Svg Msg)
svgLine maybeLine =
    maybeLine
        |> Maybe.map
            (\{ s, e, time, fixed } ->
                [ Svg.line
                    [ x1 (s |> getX |> String.fromFloat)
                    , y1 (s |> getY |> String.fromFloat)
                    , x2 (e |> getX |> String.fromFloat)
                    , y2 (e |> getY |> String.fromFloat)
                    , stroke "black"
                    ]
                    []
                ]
            )
        |> Maybe.withDefault []


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


svgPortal : Portal -> List (Svg Msg)
svgPortal portal =
    [ Svg.circle
        [ cx (portal.entrance |> Math.Vector2.getX |> round |> String.fromInt)
        , cy (portal.entrance |> Math.Vector2.getY |> round |> String.fromInt)
        , r "20"
        , fill "yellow"
        ]
        []
    , Svg.circle
        [ cx (portal.exit |> Math.Vector2.getX |> round |> String.fromInt)
        , cy (portal.exit |> Math.Vector2.getY |> round |> String.fromInt)
        , r "20"
        , fill "yellow"
        ]
        []
    ]


svgBall : Ball -> Float -> List (Svg Msg)
svgBall ball time =
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
            , r "11"
            , fill ball.color
            ]
            []
        ]
