module Main exposing (..)

import Browser
import Browser.Events
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
    { t0 : Maybe Time.Posix
    , relativeTime : Float
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
            vec2 0.1 0.2

        x0 =
            vec2 15 15

        acc =
            accFromV v0
    in
    ( Model Nothing 0 v0 x0 acc False Nothing
    , Task.perform SetTime Time.now
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | SetTime Time.Posix
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
                    model.t0
                        |> Maybe.map (\t0 -> { model | relativeTime = (Time.posixToMillis newTime - Time.posixToMillis t0) |> toFloat })
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
            ( { model | paused = not paused }, Cmd.none )

        MouseDownEvent event ->
            ( { model | line = Just ( event, event ) }, Cmd.none )

        MouseMoveEvent event ->
            ( { model | line = model.line |> Maybe.map (\( s, e ) -> ( s, event )) }, Cmd.none )

        MouseUpEvent event ->
            ( { model | line = Nothing }, Cmd.none )

        MouseLeaveEvent event ->
            ( { model | line = Nothing }, Cmd.none )



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
            Math.Vector2.length model.v0 / Math.Vector2.length model.acc

        endPos =
            model.x0 |> add (model.v0 |> Math.Vector2.scale duration) |> add (model.acc |> Math.Vector2.scale ((duration ^ 2) * 0.5))

        movements =
            [ { endPos = endPos, endTime = duration |> round, velocityRatio = 0 } ]

        stringArgs =
            movementsToSvgPath model.x0 movements

        xString =
            model.x0 |> Math.Vector2.getX |> round |> String.fromInt

        yString =
            model.x0 |> Math.Vector2.getX |> round |> String.fromInt

        maxRange =
            ceiling duration

        relTimeString =
            Basics.min model.relativeTime (maxRange |> toFloat) |> String.fromFloat
    in
    div [ H.style "display" "grid", H.style "grid-template-rows" "max-content max-content 1fr", H.style "height" "100%", H.style "width" "100%" ]
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
            ([ Svg.circle
                [ cx xString
                , cy yString
                , r "10"
                , fill "red"
                ]
                [ Svg.animateMotion
                    [ Svg.Attributes.path stringArgs.path
                    , keyPoints stringArgs.keyPoints
                    , keyTimes stringArgs.keyTimes
                    , keySplines stringArgs.keySplines
                    , calcMode "spline"
                    , fill "freeze"
                    , dur stringArgs.dur
                    ]
                    []
                ]
             ]
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


type alias BallMovement =
    { endPos : Vec2
    , endTime : Int
    , velocityRatio : Float
    }


type alias SvgMotion =
    { path : String
    , keyPoints : String
    , keyTimes : String
    , keySplines : String
    , dur : String
    }


movementsToSvgPath : Vec2 -> List BallMovement -> SvgMotion
movementsToSvgPath startPos movements =
    let
        totals =
            movements
                |> List.foldl
                    (\move ->
                        \{ distance, duration, lastPos } ->
                            { distance = distance + Math.Vector2.distance lastPos move.endPos
                            , duration = move.endTime
                            , lastPos = move.endPos
                            }
                    )
                    { distance = 0, duration = 0, lastPos = startPos }

        distanceT =
            totals.distance

        durationT =
            totals.duration

        lists =
            movements
                |> List.foldl
                    (\move ->
                        \{ path, keyPoints, keyTimes, keySplines, lastPos, lastTime } ->
                            { path = move.endPos :: path
                            , keyPoints =
                                let
                                    kpDiff =
                                        if distanceT > 0 then
                                            Math.Vector2.distance lastPos move.endPos / distanceT

                                        else
                                            1.0

                                    kp =
                                        kpDiff + (keyPoints |> List.head |> Maybe.withDefault 0.0)
                                in
                                kp :: keyPoints
                            , keyTimes =
                                let
                                    ktDiff =
                                        if durationT > 0 then
                                            toFloat (move.endTime - lastTime) / toFloat durationT

                                        else
                                            1.0

                                    kt =
                                        ktDiff + (keyTimes |> List.head |> Maybe.withDefault 0.0)
                                in
                                kt :: keyTimes
                            , keySplines =
                                let
                                    v =
                                        vec2 (1.0 / 3.0) (2.0 / (3.0 * (1.0 + move.velocityRatio)))

                                    spline =
                                        ( v, Math.Vector2.add v (vec2 (1.0 / 3.0) (1.0 / 3.0)) )
                                in
                                spline :: keySplines
                            , lastPos = move.endPos
                            , lastTime = move.endTime
                            }
                    )
                    { path = [ startPos ], keyPoints = [ 0.0 ], keyTimes = [ 0.0 ], keySplines = [], lastPos = startPos, lastTime = 0 }
    in
    { path =
        "M"
            :: (lists.path
                    |> List.reverse
                    |> List.map (\v -> [ getX v, getY v ] |> List.map (round >> String.fromInt) |> String.join " ")
                    |> List.intersperse "L"
               )
            |> String.join " "
    , keyPoints = lists.keyPoints |> List.reverse |> List.map String.fromFloat |> String.join ";"
    , keyTimes = lists.keyTimes |> List.reverse |> List.map String.fromFloat |> String.join ";"
    , keySplines =
        lists.keySplines
            |> List.reverse
            |> List.map (\( v1, v2 ) -> [ getX v1, getY v1, getX v2, getY v2 ] |> List.map String.fromFloat |> String.join " ")
            |> String.join " ; "
    , dur = String.concat [ totals.duration |> String.fromInt, "ms" ]
    }
