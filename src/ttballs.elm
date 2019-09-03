import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 exposing (vec2, Vec2, normalize, scale, add)

import Browser
import Browser.Events
import Html exposing (..)
import Task
import Time
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
  { t0 : Maybe Time.Posix,
    relativeTime : Float,
    v0: Vec2,
    x0: Vec2,
    acc: Vec2
  }

accFromV: Vec2 -> Vec2
accFromV = \v ->
  Debug.log "acc" (v |> normalize |> scale -0.00005)

init : () -> (Model, Cmd Msg)
init _ =
  let
    v0 = vec2 0.1 0.2
    x0 = vec2 15 15
    acc = accFromV v0
  in
    ( Model Nothing 0 v0 x0 acc
    , Task.perform SetTime Time.now
    )



-- UPDATE


type Msg
  = Tick Time.Posix
  | SetTime Time.Posix



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      let
          newModel = model.t0
            |> Maybe.map (\t0 -> { model | relativeTime = ((Time.posixToMillis newTime) - (Time.posixToMillis t0)) |> toFloat })
            |> Maybe.withDefault model
      in
      
      ( newModel
      , Cmd.none
      )

    SetTime t ->
      ( { model | t0 = Just t  }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Browser.Events.onAnimationFrame Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    pos = model.x0 |> add (model.v0 |> scale model.relativeTime ) |> add (model.acc |> scale ((model.relativeTime ^ 2) * 0.5))
    xString = pos |> Math.Vector2.getX |> round |> String.fromInt
    yString = pos |> Math.Vector2.getX |> round |> String.fromInt
  in
    svg [Svg.Attributes.style "position:fixed; top:0; left:0; height:100%; width:100%"] [Svg.circle [ cx xString, cy yString, r "10"] []]