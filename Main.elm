import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


type alias Bird =
  { y : Float
  , vy : Float
  , state : BirdState
  }


type alias Obstacle =
  { x : Float
  , height : Float
  }


type BirdState = Alive | Dead


type alias Model =
  { bird : Bird
  , obstacles : List Obstacle
}


init : Model
init =
  { bird = {y = 50, vy = 0, state = Alive}
  , obstacles = [{x = 100, height = 100}]
  }

physics : Float -> Model -> Model
physics dt state =
    let
        newbird = state.bird
    in
    { state | bird = { newbird |
        y = min 0 state.bird.y + state.bird.vy * dt } }



collision : Model -> Model
collision state =
    let
        has_collided ob = ob.x <= 10 && ob.x < -10 && ob.height > state.bird.y
        collided = List.filter has_collided state.obstacles
        new_bird = state.bird
    in case collided of
        [] -> state
        ob -> {state | bird = {new_bird | state = Dead }}

update : (Float, Bool) -> Model -> Model
update (dt, space) state =
  case state.bird.state of
      Dead -> state
      Alive ->
          {state | obstacles = List.map (\ob -> {ob | x = ob.x - dt}) state.obstacles} |>
          collision |>
          physics dt


ground : Float -> Float -> List Form
ground w h =
  [ rect w h
      |> filled (rgb 174 238 238)
  , rect w 50
      |> filled (rgb 74 167 43)
      |> move (0, 24 - h/2)
  ]

drawMario : Float -> Float -> Form
drawMario h birdH = rect 20 20 |> filled (rgb 50 255 50) |> moveY (-h/2.0 + 62.0 - birdH)


drawObstacles : List Obstacle -> List Form
drawObstacles obstacles =
  List.map (\ob -> rect 10 ob.height
        |> filled (rgb 255 50 50)
        |> move (ob.x, 0)) obstacles


view : (Int, Int) -> Model -> Element
view (w',h') model =
  let
    (w,h) = (toFloat w', toFloat h')

    groundHeight = 62.0
    groundY = groundHeight - h/2
    obstacleRects : List Form
    obstacleRects =
      -- Note: Should translate by half obstacle height here
      List.map (\r -> moveY (-h/2.0 + 62.0) r) (drawObstacles model.obstacles)

  in
    collage w' h' ((ground w h) ++ obstacleRects ++ [drawMario h model.bird.y])


main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update init input)


input : Signal (Float, Bool)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.space)
