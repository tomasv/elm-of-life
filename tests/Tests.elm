module Tests exposing (..)

import Test exposing (..)
import Expect

import GameOfLife

all : Test
all =
    describe "GameOfLife"
        [ test "no cells" <|
            \() ->
                Expect.equal [] (GameOfLife.nextGeneration [])
        , test "horizontal stick spinner" <|
            \() ->
                let
                    startGeneration = [(1,0), (2,0), (3,0)]
                    endGeneration = [(2,-1), (2,0), (2,1)]
                in
                    Expect.equal endGeneration (GameOfLife.nextGeneration startGeneration)
        , test "vertical stick spinner" <|
            \() ->
                let
                    startGeneration = [(0,1), (0,2), (0,3)]
                    endGeneration = [(-1,2), (0,2), (1,2)]
                in
                    Expect.equal endGeneration (GameOfLife.nextGeneration startGeneration)
        ]
