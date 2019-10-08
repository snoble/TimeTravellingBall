module Main exposing (..)

import Aberth exposing (solve)
import Browser
import Browser.Dom as Dom
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
import Maybe.Extra exposing (orElse)
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
    { endPos : Maybe Vec2
    , startVelocity : Maybe VelAcc
    , duration : Float
    }


type BallPosition
    = OnBoard BallOnBoard
    | Vanished Float


type alias BallOnBoard =
    { t : Float
    , x : Vec2
    , va : VelAcc
    }


posXVA : BallPosition -> Maybe ( Vec2, VelAcc )
posXVA pos =
    case pos of
        Vanished _ ->
            Nothing

        OnBoard { t, x, va } ->
            Just ( x, va )


posT : BallPosition -> Float
posT pos =
    case pos of
        Vanished t ->
            t

        OnBoard { t, x, va } ->
            t


type BallType
    = Normal
    | Ghost
    | Substance


type alias Ball =
    { initPosition : Maybe Vec2
    , initTime : Float
    , movements : List BallMovement
    , ballType : BallType
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


matrix4Tofn2 : Math.Matrix4.Mat4 -> Vec2 -> Vec2
matrix4Tofn2 matrix =
    \v2 ->
        let
            v3 =
                Math.Matrix4.transform matrix (vec3 (getX v2) (getY v2) 0)
        in
        vec2 (Math.Vector3.getX v3) (Math.Vector3.getY v3)


createRotation : Float -> Vec2 -> Vec2
createRotation angle =
    let
        matrix =
            Math.Matrix4.makeRotate angle (vec3 0 0 1)
    in
    matrix4Tofn2 matrix


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


type alias DurationAnimation =
    { start : Int
    , end : Int
    , startTime : Time.Posix
    }


type alias DisplayScaling =
    { toDisplay : Vec2 -> Vec2
    , fromDisplay : Vec2 -> Vec2
    }


type alias Model =
    { relativeTime : Int
    , playTime : Maybe Time.Posix
    , line : Maybe Line
    , balls : List Ball
    , duration : Int
    , portal : Portal
    , initBall : ( BallType, BallPosition )
    , ghosts : List Ball
    , targetDuration : Maybe DurationAnimation
    , viewport : Maybe Dom.Viewport
    , displayScaling : Maybe DisplayScaling
    , spaceHeight : Float
    , spaceWidth : Float
    }


zeroTimePosition : BallOnBoard -> BallOnBoard
zeroTimePosition pos =
    ballPosAtT pos 0.0


createBalls : List ( BallType, BallPosition ) -> List Portal -> ( List Ball, List Ball )
createBalls initialState portals =
    let
        initDict =
            initialState
                |> List.indexedMap (\i -> \( _, pos ) -> ( i, NE.fromElement pos ))
                |> Dict.fromList

        ( movementDict, ghostDict ) =
            ( initDict, Dict.empty ) |> generatePositions portals

        movementData =
            movementDict
                |> Dict.values
                |> List.map movementsFromPositions

        liveBalls =
            List.map2
                (\( initialPosition, t, movements ) ->
                    \( ballType, _ ) ->
                        { initPosition = initialPosition
                        , initTime = t
                        , movements = movements
                        , ballType = Normal
                        }
                )
                movementData
                initialState

        ghosts =
            ghostDict
                |> Dict.values
                |> List.map
                    (\positions ->
                        let
                            ( initialPosition, t, movements ) =
                                movementsFromPositions positions
                        in
                        { initPosition = initialPosition
                        , initTime = t
                        , movements = movements
                        , ballType = Ghost
                        }
                    )
    in
    ( liveBalls, ghosts )



-- assumes the last postion is the stopped ball


movementsFromPositions : Nonempty BallPosition -> ( Maybe Vec2, Float, List BallMovement )
movementsFromPositions positions =
    let
        firstP =
            positions |> NE.head

        tail =
            positions |> NE.tail

        initPosition =
            firstP |> posXVA |> Maybe.map Tuple.first

        initTime =
            firstP |> posT

        movements =
            tail
                |> List.foldl
                    (\thisP ->
                        \( previousP, movementsSoFar ) ->
                            ( thisP
                            , { endPos = thisP |> posXVA |> Maybe.map Tuple.first
                              , startVelocity = previousP |> posXVA |> Maybe.map Tuple.second
                              , duration = (thisP |> posT) - (previousP |> posT)
                              }
                                :: movementsSoFar
                            )
                    )
                    ( firstP, [] )
                |> Tuple.second
                |> List.reverse
    in
    ( initPosition, initTime, movements )


interceptTime : BallOnBoard -> BallOnBoard -> List Float
interceptTime position1 position2 =
    if (position1 |> endPositionForStopTime) < position2.t || (position2 |> endPositionForStopTime) < position1.t then
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
                    if abs im < eps && re >= minT && re < (position1 |> endPositionForStopTime) && re < (position2 |> endPositionForStopTime) then
                        Just (c |> toCartesian).re

                    else
                        Nothing
                )


collisionsFromPositions : IndexedBallPosition -> IndexedBallPosition -> Maybe BallCollision
collisionsFromPositions iPosition1 iPosition2 =
    case ( iPosition1.pos, iPosition2.pos ) of
        ( Vanished _, _ ) ->
            Nothing

        ( _, Vanished _ ) ->
            Nothing

        ( OnBoard position1, OnBoard position2 ) ->
            interceptTime position1 position2
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
                                            ballPosAtT position1 t

                                        candidatePosition2 =
                                            ballPosAtT position2 t

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
                                        Just (BallCollision { iPosition1 | pos = OnBoard postCollision1 } { iPosition2 | pos = OnBoard postCollision2 } t)

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


positionsAfterCollision : BallOnBoard -> BallOnBoard -> ( BallOnBoard, BallOnBoard )
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


possiblePortalJumps : List Portal -> List IndexedBallPosition -> List PortalJumpParts
possiblePortalJumps portals balls =
    portals
        |> List.concatMap
            (\portal ->
                balls
                    |> List.concatMap
                        (\ball ->
                            case ball.pos of
                                Vanished _ ->
                                    []

                                OnBoard pos ->
                                    let
                                        times =
                                            interceptTime { t = 0, x = portal.entrance, va = vaZero } pos
                                    in
                                    times
                                        |> List.map
                                            (\t ->
                                                let
                                                    enteringPos =
                                                        ballPosAtT pos t

                                                    exitingPos =
                                                        { enteringPos
                                                            | x = Math.Vector2.add portal.exit (Math.Vector2.sub enteringPos.x portal.entrance |> portal.rotation)
                                                            , va = rotateVelAcc portal.rotation enteringPos.va
                                                            , t = enteringPos.t - portal.timeDelta
                                                        }
                                                in
                                                { idx = ball.idx, t = t, pos = exitingPos }
                                            )
                        )
            )


nextChanges : List BallPosition -> List Portal -> ( List IndexedBallPosition, List IndexedBallPosition )
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
                |> List.filterMap
                    (\iPos ->
                        case iPos.pos of
                            Vanished _ ->
                                Nothing

                            OnBoard pos ->
                                if pos.va.stopTime > 0 then
                                    Just ( iPos, pos )

                                else
                                    Nothing
                    )
                |> minimumBy (\( _, pos ) -> pos |> endPositionForStopTime)
                |> Maybe.map
                    (\( iPos, pos ) ->
                        { iPos
                            | pos =
                                OnBoard
                                    { x = posAtT pos.x pos.va.stopTime pos.va
                                    , va = vaZero
                                    , t = pos.t + pos.va.stopTime
                                    }
                        }
                    )

        nextCollision =
            possibleCollisions
                |> minimumBy (\col -> col.t)

        nextPortalJump =
            possiblePortalJumps portals indexedPositions |> minimumBy (\pj -> pj.t)

        nextChange =
            [ nextStop |> Maybe.map (\ns -> ( ns.pos |> posT, Stop ns ))
            , nextCollision |> Maybe.map (\nc -> ( nc.t, Collision nc ))
            , nextPortalJump |> Maybe.map (\np -> ( np.t, PortalJump np ))
            ]
                |> List.filterMap identity
                |> minimumBy (\( t, _ ) -> t)
                |> Maybe.map Tuple.second
    in
    case nextChange of
        Nothing ->
            ( [], [] )

        Just (Stop ns) ->
            ( [ ns ], [] )

        Just (Collision nc) ->
            ( [ nc.ball1, nc.ball2 ], [] )

        Just (PortalJump { idx, t, pos }) ->
            let
                ghostAppear =
                    { idx = idx, pos = OnBoard pos }

                ghostStop =
                    { idx = idx, pos = OnBoard { x = posAtT pos.x pos.va.stopTime pos.va, va = vaZero, t = pos.t + pos.va.stopTime } }
            in
            ( [ { idx = idx, pos = Vanished t } ], [ ghostAppear, ghostStop ] )


type alias PortalJumpParts =
    { idx : Int
    , t : Float
    , pos : BallOnBoard
    }


type NextChange
    = Stop IndexedBallPosition
    | Collision BallCollision
    | PortalJump PortalJumpParts


updateBallDict : List IndexedBallPosition -> Dict Int (Nonempty BallPosition) -> Dict Int (Nonempty BallPosition)
updateBallDict indexedPositions ballDict =
    indexedPositions
        |> List.foldl
            (\{ idx, pos } ->
                Dict.update idx (Maybe.map (NE.cons pos) >> Maybe.withDefault (NE.fromElement pos) >> Just)
            )
            ballDict


generatePositions : List Portal -> ( Dict Int (Nonempty BallPosition), Dict Int (Nonempty BallPosition) ) -> ( Dict Int (Nonempty BallPosition), Dict Int (Nonempty BallPosition) )
generatePositions portals ( ballPositions, ghosts ) =
    let
        ( newPositions, newGhosts ) =
            nextChanges (ballPositions |> Dict.values |> List.map NE.head) portals

        resultGhosts =
            ghosts |> updateBallDict newGhosts

        resultPositions =
            ballPositions |> updateBallDict newPositions
    in
    case newPositions of
        [] ->
            ( resultPositions |> Dict.map (\_ -> NE.reverse), resultGhosts |> Dict.map (\_ -> NE.reverse) )

        _ ->
            generatePositions portals
                ( resultPositions, resultGhosts )


endPositionForStopTime : BallOnBoard -> Float
endPositionForStopTime pos =
    if pos.va.stopTime == 0.0 then
        1 / 0

    else
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


indexedPositionFrom : Vec2 -> Vec2 -> Float -> Int -> IndexedBallPosition
indexedPositionFrom x v t idx =
    let
        va =
            avFromV v
    in
    { idx = idx
    , pos =
        OnBoard
            { t = t
            , x = x
            , va = va
            }
    }


durationFromBalls : List Ball -> List Ball -> Int
durationFromBalls balls ghosts =
    let
        maxDuration =
            (balls ++ ghosts)
                |> List.map
                    (\ball ->
                        ball.movements
                            |> List.foldl (\mvmt -> \totalDur -> totalDur + mvmt.duration) ball.initTime
                    )
                |> List.maximum
                |> Maybe.withDefault 0.0
    in
    ceiling (maxDuration / 3000.0) * 3000


init : () -> ( Model, Cmd Msg )
init _ =
    let
        v1 =
            vec2 0.18 0.7 |> Math.Vector2.scale 0.65

        initBall =
            ( Normal, OnBoard { t = 1000.0, x = vec2 300 100, va = avFromV v1 } )

        portal =
            createPortal (vec2 500 800) (vec2 100 800) (3 * pi / 2) 2500

        ( balls, ghosts ) =
            createBalls
                [ initBall
                ]
                [ portal ]

        duration =
            durationFromBalls balls ghosts
    in
    ( { relativeTime = 0
      , playTime = Nothing
      , line = Nothing
      , initBall = initBall
      , balls = balls
      , duration = duration
      , portal = portal
      , ghosts = ghosts
      , targetDuration = Nothing
      , viewport = Nothing
      , displayScaling = Nothing
      , spaceHeight = 900
      , spaceWidth = 600
      }
    , Cmd.batch
        [ Task.perform Play Time.now
        , Task.perform CurrentViewport Dom.getViewport
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Play Time.Posix
    | MouseDownEvent DisplayScaling Mouse.Event
    | MouseMoveEvent DisplayScaling Mouse.Event
    | MouseUpEvent Mouse.Event
    | MouseLeaveEvent Mouse.Event
    | ChangeLineTime (Maybe Int)
    | SetTargetDuration Int Time.Posix
    | CurrentViewport Dom.Viewport
    | Resize


pe2Vec2 : DisplayScaling -> Mouse.Event -> Vec2
pe2Vec2 ds event =
    let
        ( x, y ) =
            event.pointer.offsetPos
    in
    vec2 x y |> ds.fromDisplay


vecToExitStartingPoint : Portal -> Vec2 -> Maybe Vec2
vecToExitStartingPoint portal x =
    if Math.Vector2.distance x portal.exit > 50.0 then
        Nothing

    else
        let
            direction =
                if Math.Vector2.distance x portal.exit == 0.0 then
                    vec2 1 0

                else
                    Math.Vector2.direction x portal.exit
        in
        Just (Math.Vector2.add portal.exit (Math.Vector2.scale 20.0 direction))


updateWithNewLine : Model -> Line -> ( Model, Cmd Msg )
updateWithNewLine model line =
    let
        distance =
            Math.Vector2.distance line.e line.s

        initV =
            Math.Vector2.direction line.e line.s |> Math.Vector2.scale (sqrt (distance * accIntensity * 2.0))

        pos2 =
            { t = line.time, x = line.s, va = avFromV initV }

        ( balls, ghosts ) =
            createBalls [ model.initBall, ( Substance, OnBoard pos2 ) ] [ model.portal ]

        targetDuration =
            durationFromBalls balls ghosts
    in
    ( { model | line = Just line, balls = balls, ghosts = ghosts }, Task.perform (SetTargetDuration targetDuration) Time.now )


updateDuration : Model -> Time.Posix -> Model
updateDuration model newTime =
    case model.targetDuration of
        Nothing ->
            model

        Just durationAnimation ->
            let
                timePassed =
                    (Time.posixToMillis newTime - Time.posixToMillis durationAnimation.startTime) |> toFloat

                diff =
                    (durationAnimation.end - durationAnimation.start) |> toFloat

                rate =
                    1.5

                ( duration, targetDuration ) =
                    if timePassed * rate >= abs diff then
                        ( durationAnimation.end, Nothing )

                    else
                        ( durationAnimation.start + round (timePassed * rate * diff / abs diff), Just durationAnimation )
            in
            { model | duration = duration, targetDuration = targetDuration }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                modelDur =
                    updateDuration model newTime

                newModel =
                    modelDur.playTime
                        |> Maybe.map
                            (\t0 ->
                                let
                                    rt =
                                        Time.posixToMillis newTime - Time.posixToMillis t0

                                    ( relativeTime, playTime ) =
                                        if rt > modelDur.duration then
                                            ( 0, Just newTime )

                                        else
                                            ( rt, Just t0 )
                                in
                                { modelDur | relativeTime = relativeTime, playTime = playTime }
                            )
            in
            ( newModel |> Maybe.withDefault modelDur, Cmd.none )

        Play t ->
            ( { model | playTime = Just (Time.posixToMillis t - model.relativeTime |> Time.millisToPosix) }
            , Cmd.none
            )

        MouseDownEvent ds event ->
            ( if event.isPrimary then
                let
                    v =
                        pe2Vec2 ds event

                    line =
                        v
                            |> vecToExitStartingPoint model.portal
                            |> Maybe.map
                                (\s ->
                                    { s = s
                                    , e = v
                                    , time = model.line |> Maybe.map (\l -> l.time) |> Maybe.withDefault (model.relativeTime |> toFloat)
                                    , fixed = False
                                    }
                                )
                            |> orElse model.line
                in
                { model | line = line }

              else
                model
            , Cmd.none
            )

        MouseMoveEvent ds event ->
            if event.isPrimary then
                let
                    newLine =
                        model.line
                            |> Maybe.map
                                (\line ->
                                    if line.fixed then
                                        line

                                    else
                                        { line | e = pe2Vec2 ds event }
                                )
                in
                case newLine of
                    Just line ->
                        updateWithNewLine model line

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        MouseUpEvent event ->
            if event.isPrimary then
                case model.line of
                    Just line ->
                        updateWithNewLine model { line | fixed = True, time = model.line |> Maybe.map (\l -> l.time) |> Maybe.withDefault line.time }

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        MouseLeaveEvent event ->
            if event.isPrimary then
                case model.line of
                    Just line ->
                        updateWithNewLine model { line | fixed = True, time = model.line |> Maybe.map (\l -> l.time) |> Maybe.withDefault line.time }

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        ChangeLineTime lt ->
            let
                ( newModel, cmd ) =
                    Maybe.map2
                        (\t ->
                            \line ->
                                updateWithNewLine model { line | time = t }
                        )
                        (lt |> Maybe.map toFloat)
                        model.line
                        |> Maybe.withDefault ( model, Cmd.none )
            in
            ( newModel, cmd )

        SetTargetDuration duration t ->
            ( { model | targetDuration = Just { start = model.duration, end = duration, startTime = t } }, Cmd.none )

        CurrentViewport vp ->
            let
                scaler =
                    Basics.min
                        (vp.viewport.height / model.spaceHeight)
                        (vp.viewport.width / model.spaceWidth)

                invScaler =
                    1 / scaler

                fromDisplay =
                    matrix4Tofn2 (Math.Matrix4.makeScale (vec3 invScaler invScaler 1))

                toDisplay =
                    matrix4Tofn2 (Math.Matrix4.makeScale (vec3 scaler scaler 1))

                displayScaling =
                    { fromDisplay = fromDisplay, toDisplay = toDisplay }
            in
            ( { model | viewport = Just vp, displayScaling = Just displayScaling }, Cmd.none )

        Resize ->
            ( model, Task.perform CurrentViewport Dom.getViewport )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrame Tick
        , Browser.Events.onResize (\_ -> \_ -> Resize)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        maxRange =
            model.duration

        relTimeString =
            Basics.min model.relativeTime maxRange |> String.fromInt

        ( svgObjects, svgAttributes ) =
            case model.displayScaling of
                Nothing ->
                    ( [], [] )

                Just ds ->
                    ( (model.ghosts |> List.concatMap (\ball -> svgBall ds ball (model.relativeTime |> toFloat)))
                        ++ (model.balls |> List.concatMap (\ball -> svgBall ds ball (model.relativeTime |> toFloat)))
                        ++ svgPortal ds model.portal
                        ++ svgLine ds model.line
                    , [ Mouse.onDown (MouseDownEvent ds)
                      , Mouse.onMove (MouseMoveEvent ds)
                      , Mouse.onUp MouseUpEvent
                      , Mouse.onLeave MouseLeaveEvent
                      ]
                    )
    in
    div
        [ H.style "display" "grid"
        , H.style "grid-template-rows" "max-content 2em 1fr"
        , H.style "height" "100%"
        , H.style "width" "100%"
        ]
        [ input
            [ H.type_ "range"
            , H.min "0"
            , H.max (maxRange |> String.fromInt)
            , H.value relTimeString
            , H.readonly True
            ]
            []
        , case model.line of
            Just _ ->
                input
                    [ H.type_ "range"
                    , H.min "0"
                    , H.max (maxRange |> String.fromInt)
                    , H.readonly True
                    , H.value (model.line |> Maybe.map (\l -> l.time) |> Maybe.withDefault 0 |> String.fromFloat)
                    , onInput (String.toInt >> ChangeLineTime)
                    ]
                    []

            Nothing ->
                div [] []
        , svg
            (Svg.Attributes.style "height:100%; width:100%" :: svgAttributes)
            svgObjects
        ]


svgLine : DisplayScaling -> Maybe Line -> List (Svg Msg)
svgLine ds maybeLine =
    maybeLine
        |> Maybe.map
            (\{ s, e, time, fixed } ->
                let
                    scaledS =
                        ds.toDisplay s

                    scaledE =
                        ds.toDisplay e

                    x1Str =
                        scaledS |> getX |> String.fromFloat

                    y1Str =
                        scaledS |> getY |> String.fromFloat

                    x2Str =
                        scaledE |> getX |> String.fromFloat

                    y2Str =
                        scaledE |> getY |> String.fromFloat

                    radius =
                        vec2 3 0 |> ds.toDisplay |> getX
                in
                [ Svg.line
                    [ x1 x1Str
                    , y1 y1Str
                    , x2 x2Str
                    , y2 y2Str
                    , stroke "black"
                    ]
                    []
                , Svg.circle
                    [ cx x1Str
                    , cy y1Str
                    , r (radius |> String.fromFloat)
                    , fill "black"
                    ]
                    []
                , Svg.circle
                    [ cx x2Str
                    , cy y2Str
                    , r (radius |> String.fromFloat)
                    , fill "black"
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
        , stopTime = va.stopTime - t
    }


ballPosAtT : BallOnBoard -> Float -> BallOnBoard
ballPosAtT ball t =
    { t = t
    , x = posAtT ball.x (t - ball.t) ball.va
    , va = vaAtT ball.va (t - ball.t)
    }


svgPortal : DisplayScaling -> Portal -> List (Svg Msg)
svgPortal ds portal =
    let
        outerRadius =
            vec2 32 0 |> ds.toDisplay |> getX

        innerRadius =
            vec2 10 0 |> ds.toDisplay |> getX

        entrance =
            portal.entrance |> ds.toDisplay

        exit =
            portal.exit |> ds.toDisplay
    in
    [ Svg.radialGradient
        [ id "portalGradient"
        ]
        [ Svg.stop [ Svg.Attributes.offset "5%", Svg.Attributes.stopColor "blue" ] [], Svg.stop [ Svg.Attributes.offset "95%", Svg.Attributes.stopColor "yellow" ] [] ]
    , Svg.circle
        [ cx (entrance |> Math.Vector2.getX |> round |> String.fromInt)
        , cy (entrance |> Math.Vector2.getY |> round |> String.fromInt)
        , r (outerRadius |> String.fromFloat)
        , fill "url('#portalGradient')"
        ]
        []
    , Svg.mask [ id "exitMask" ]
        [ Svg.rect
            [ x (((exit |> Math.Vector2.getX) - outerRadius) |> String.fromFloat)
            , y (((exit |> Math.Vector2.getY) - outerRadius) |> String.fromFloat)
            , width (outerRadius * 2 |> String.fromFloat)
            , height (outerRadius * 2 |> String.fromFloat)
            , fill "white"
            ]
            []
        , Svg.circle
            [ cx (exit |> Math.Vector2.getX |> round |> String.fromInt)
            , cy (exit |> Math.Vector2.getY |> round |> String.fromInt)
            , r (innerRadius |> String.fromFloat)
            , fill "black"
            ]
            []
        ]
    , Svg.circle
        [ cx (exit |> Math.Vector2.getX |> round |> String.fromInt)
        , cy (exit |> Math.Vector2.getY |> round |> String.fromInt)
        , r (outerRadius |> String.fromFloat)
        , fill "url('#portalGradient')"
        , Svg.Attributes.mask "url(#exitMask)"
        ]
        []
    ]


svgBall : DisplayScaling -> Ball -> Float -> List (Svg Msg)
svgBall ds ball time =
    if time < ball.initTime then
        []

    else
        let
            timeAdjusted =
                time - ball.initTime

            movements =
                ball.movements

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
                        { t = timeAdjusted, startPos = ball.initPosition, va = Just vaZero, done = False }

            pos =
                Maybe.map2 (\sp -> \vva -> posAtT sp t vva |> ds.toDisplay) startPos va

            xString =
                pos |> Maybe.map (Math.Vector2.getX >> round >> String.fromInt)

            yString =
                pos |> Maybe.map (Math.Vector2.getY >> round >> String.fromInt)

            xys =
                [ Maybe.map2 (\xs -> \ys -> ( xs, ys )) xString yString ] |> List.filterMap identity

            radius =
                vec2 11 0 |> ds.toDisplay |> getX |> String.fromFloat
        in
        xys
            |> List.concatMap
                (\( xs, ys ) ->
                    svgBallType radius xs ys ball.ballType
                )


svgBallType : String -> String -> String -> BallType -> List (Svg Msg)
svgBallType radius xString yString ballType =
    [ Svg.circle
        [ cx xString
        , cy yString
        , r radius
        , fill "darkred"
        ]
        []
    , Svg.circle
        [ cx xString
        , cy yString
        , r radius
        , fill "red"
        ]
        []
    ]
