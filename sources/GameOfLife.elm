module GameOfLife exposing (nextGeneration, Cell)

import Dict exposing (Dict)

-- Rules of Conways Game of Life

-- At each step in time, the following transitions occur:
-- Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by overpopulation.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

type alias Cell = (Int, Int)

nextGeneration : List Cell -> List Cell
nextGeneration aliveCells =
  aliveCells
    |> List.concatMap (neighbours)
    |> List.foldl (count) Dict.empty
    |> Dict.filter (isAlive aliveCells)
    |> Dict.keys

neighbours : Cell -> List Cell
neighbours (x, y) = 
  [ (x - 1, y + 1), (x    , y + 1), (x + 1, y + 1)
  , (x - 1, y),                     (x + 1, y)
  , (x - 1, y - 1), (x    , y - 1), (x + 1, y - 1)
  ]

isAlive : List Cell -> Cell -> Int -> Bool
isAlive aliveCells cell liveCount =
  (liveCount == 3) || ((liveCount == 2) && (List.member cell aliveCells))

count : Cell -> Dict Cell Int -> Dict Cell Int
count key freqs =
  Dict.update key (increment) freqs

increment : Maybe Int -> Maybe Int
increment value =
    case value of
      Just x -> Just (x + 1)
      Nothing -> Just 1
