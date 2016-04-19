import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


type alias Bird =
  { y : Float
  , vy : Float
  }


type alias Obstacle =
  { x : Float
  , height : Float
  }


type alias Model =
  { bird : Bird
  , obstacles : List Obstacle
  }


init : Model
init =
  { bird = {y = 50, vy = 0}
  , obstacles = [{x = 0, height = 100}]
  }


update : (Float, Bool) -> Model -> Model
update (dt, space) state =
  init


ground : Float -> Float -> List Form
ground w h =
  [ rect w h
      |> filled (rgb 174 238 238)
  , rect w 50
      |> filled (rgb 74 167 43)
      |> move (0, 24 - h/2)
  ]


view : (Int, Int) -> Model -> Element
view (w',h') mario =
  let
    (w,h) = (toFloat w', toFloat h')

    groundY = 62 - h/2


  in
    collage w' h' (ground w h)


main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update init input)


input : Signal (Float, Bool)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.space)
