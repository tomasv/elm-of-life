import Html exposing (..)
import Html.Events exposing (..)
import Set exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Cell = (Int, Int)

type Msg = NextGeneration

init = Set.fromList [(0, 1), (1, 1), (2, 1)] ! []

update msg model =
  case msg of
    NextGeneration -> (nextGeneration model, Cmd.none)

-- Rules of Conways Game of Life

-- At each step in time, the following transitions occur:
-- Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by overpopulation.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

neighbours : Cell -> List Cell
neighbours (x, y) = 
  [ (x - 1, y + 1), (x    , y + 1), (x + 1, y + 1)
  , (x - 1, y),                     (x + 1, y)
  , (x - 1, y - 1), (x    , y - 1), (x + 1, y - 1)
  ]

aliveOrDead : List Cell -> Cell -> Maybe Cell
aliveOrDead aliveCells candidate =
  let
      aliveCellSet = Set.fromList aliveCells
      neighbourCells = Set.fromList (neighbours candidate)
      liveNeighbours = Set.intersect aliveCellSet neighbourCells
      liveCount = Set.size liveNeighbours
      isDead = not (Set.member candidate aliveCellSet)
  in
      if liveCount < 2 || liveCount > 3 || (isDead && liveCount == 2) then
        Nothing
      else
        Just candidate

nextGeneration : Set Cell -> Set Cell
nextGeneration model =
  let
      aliveCells = Set.toList model
      candidateCells = aliveCells ++ (List.concatMap (neighbours) aliveCells)
      newGeneration = List.filterMap (aliveOrDead aliveCells) candidateCells 
  in
      Set.fromList newGeneration

subscriptions model =
  Sub.none

view model =
  div []
    [ div [] [text (toString model)]
    , button [onClick NextGeneration] [text "Step"]
    ]
