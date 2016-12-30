module GameOfLife exposing (nextGeneration, Cell)

import Set exposing (..)

-- Rules of Conways Game of Life

-- At each step in time, the following transitions occur:
-- Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by overpopulation.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

type alias Cell = (Int, Int)

nextGeneration : List Cell -> List Cell
nextGeneration model =
  let
      candidateCells = model ++ (List.concatMap (neighbours) model)
  in
      uniq (List.filterMap (aliveOrDead model) candidateCells)

neighbours : Cell -> List Cell
neighbours (x, y) = 
  [ (x - 1, y + 1), (x    , y + 1), (x + 1, y + 1)
  , (x - 1, y),                     (x + 1, y)
  , (x - 1, y - 1), (x    , y - 1), (x + 1, y - 1)
  ]

aliveOrDead : List Cell -> Cell -> Maybe Cell
aliveOrDead aliveCells candidate =
  let
      neighbourCells = neighbours candidate
      liveNeighbours = intersection aliveCells neighbourCells
      liveCount = List.length liveNeighbours
      isDead = not (List.member candidate aliveCells)
  in
      if liveCount < 2 || liveCount > 3 || (isDead && liveCount == 2) then
        Nothing
      else
        Just candidate

intersection : List Cell -> List Cell -> List Cell
intersection a b = Set.toList (Set.intersect (Set.fromList a) (Set.fromList b))

uniq : List Cell -> List Cell
uniq a = Set.toList (Set.fromList a)
