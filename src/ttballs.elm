module Main exposing (..)

import Browser
import Browser.Events
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


type alias BallMovement =
    { endPos : Vec2
    , startVelocity : Vec2
    , duration : Float
    }


type alias BallPosition =
    { t : Float
    , x : Vec2
    , v : Vec2
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
    { otherBall : Int
    , t : Float
    }


type alias IndexedBallPosition =
    { idx : Int
    , pos : BallPosition
    , stopTime : Float
    }


type alias Model =
    { relativeTime : Int
    , playTime : Maybe Time.Posix
    , paused : Bool
    , line : Maybe ( Mouse.Event, Mouse.Event )
    , balls : List Ball
    , duration : Float
    }


collisionFromPositions : IndexedBallPosition -> IndexedBallPosition -> List BallCollision
collisionFromPositions position1 position2 =
    if position1.stopTime < position2.pos.t || position2.stopTime < position1.pos.t then
        []

    else
        let
            t =
                Basics.max position1.pos.t position2.pos.t
        in
        []


collisionCandidates : IndexedBallPosition -> List ( IndexedBallPosition, List BallCollision ) -> List ( IndexedBallPosition, List BallCollision )
collisionCandidates position otherPositions =
    let
        newCollisions =
            otherPositions
                |> List.concatMap
                    (\( otherPosition, otherCollisions ) ->
                        collisionFromPositions position otherPosition
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
                                    pos.t + (Math.Vector2.length pos.v / 0.0001)
                            in
                            IndexedBallPosition idx pos stopTime
                    )
                |> List.foldl collisionCandidates []
    in
    ( [], [] )


accFromV : Vec2 -> Vec2
accFromV =
    normalize >> Math.Vector2.scale -0.0001


movementFrom : Vec2 -> Vec2 -> BallMovement
movementFrom x0 v0 =
    let
        duration =
            Math.Vector2.length v0 / 0.0001
    in
    { endPos = posAtT x0 duration v0 0.0001
    , startVelocity = v0
    , duration = duration
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        balls =
            [ { color = "red"
              , initPosition = vec2 100 100
              , initTime = 1500.0
              , movements =
                    Just
                        [ movementFrom (vec2 100 100) (vec2 0.2 0.3)
                        ]
              }
            , { color = "blue"
              , initPosition = vec2 100 800
              , initTime = 500.0
              , movements =
                    Just
                        [ movementFrom (vec2 100 800) (vec2 0.1 -0.35)
                        ]
              }
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
            , H.max (duration |> ceiling |> String.fromInt)
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
            ((model.balls |> List.concatMap (\ball -> svgCircle ball (model.relativeTime |> toFloat) -0.0001))
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


svgCircle : Ball -> Float -> Float -> List (Svg Msg)
svgCircle ball time decelCoef =
    if time < ball.initTime then
        []

    else
        let
            timeAdjusted =
                time - ball.initTime

            movements =
                ball.movements |> Maybe.withDefault []

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
                        { t = timeAdjusted, startPos = ball.initPosition, v = vec2 0.0 0.0, done = False }

            pos =
                posAtT startPos t v decelCoef

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
