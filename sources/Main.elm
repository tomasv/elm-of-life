import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import GameOfLife exposing (nextGeneration, Cell)
import Time exposing (Time, second)
import Keyboard exposing (..)
import Char exposing (..)
import Debug exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg = NextGeneration
  | ToggleCell Int Int
  | Tick Time
  | ToggleAutomatic
  | ClearCells
  | ChangeTickTime String
  | KeyDown Int

type alias Model = 
  { cells : List Cell
  , generation : Int
  , automatic: Bool
  , tickTime : Time
  , gridDimensions : GridDimensions
  }

defaultCells =
  [ (1, 0)
  , (2, 0)
  , (3, 0)
  , (4, 0)
  , (5, 0)
  , (6, 0)
  , (7, 0)
  , (8, 0)
  , (10, 0)
  , (11, 0)
  , (12, 0)
  , (13, 0)
  , (14, 0)
  , (18, 0)
  , (19, 0)
  , (20, 0)
  , (27, 0)
  , (28, 0)
  , (29, 0)
  , (30, 0)
  , (31, 0)
  , (32, 0)
  , (33, 0)
  , (35, 0)
  , (36, 0)
  , (37, 0)
  , (38, 0)
  , (39, 0)
  ]

defaultDimensions : GridDimensions
defaultDimensions = (0, 10, 0, 10)

init : (Model, Cmd Msg)
init = 
    { cells = defaultCells
    , generation = 1
    , automatic = False
    , tickTime = second
    , gridDimensions = dimensions defaultCells defaultDimensions
    } ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToggleAutomatic ->
      toggleAutomatic model ! []
    Tick _ ->
      update NextGeneration model
    NextGeneration ->
      advanceGeneration model ! []
    ToggleCell x y ->
      let
          newCells = toggleCell model.cells (x, y)
      in
          { model |
            cells = newCells,
            gridDimensions = dimensions newCells model.gridDimensions
          } ! []
    ClearCells ->
      clear model ! []
    ChangeTickTime time ->
      { model |
        tickTime = time
          |> String.toFloat
          |> Result.withDefault 1.0
          |> (*) second
      } ! []
    KeyDown code ->
      let
          keyName = Char.fromCode code
          newModel = handleKeyDown model keyName
      in
          newModel ! []

handleKeyDown : Model -> Char -> Model
handleKeyDown model keyName =
  case keyName of
    'S' -> advanceGeneration model
    'C' -> clear model
    'X' -> adjustTickTime model 100
    'Z' -> adjustTickTime model -100
    'P' -> toggleAutomatic model
    _ -> model

adjustTickTime : Model -> Float -> Model
adjustTickTime model increment =
  { model |
    tickTime = clamp 100 2000 (model.tickTime + increment)
  }

clear : Model -> Model
clear model =
  { model |
    cells = [],
    generation = 1,
    automatic = False
  }

advanceGeneration : Model -> Model
advanceGeneration ({cells, generation, gridDimensions} as model) =
  let
      nextGenCells = nextGeneration cells
  in
      { model |
        generation = generation + 1,
        cells = nextGenCells,
        gridDimensions = dimensions nextGenCells gridDimensions
      }

toggleCell : List Cell -> Cell -> List Cell
toggleCell aliveCells cell =
  if List.member cell aliveCells then
    List.filter (\aliveCell -> aliveCell /= cell) aliveCells
  else
    cell :: aliveCells

toggleAutomatic : Model -> Model
toggleAutomatic model =
  { model | automatic = not model.automatic }

subscriptions : Model -> Sub Msg
subscriptions model =
  let
      tickerSubs = 
        if model.automatic then
          [Time.every model.tickTime Tick]
        else
          []
      subs = [ Keyboard.downs KeyDown ] ++ tickerSubs
  in
      Sub.batch subs

view : Model -> Html Msg
view model =
  let
      cellGrid = grid model
      rows = createRows cellGrid
      automaticText = if model.automatic then "Stop" else "Start"
  in
      div []
        [ button [onClick NextGeneration] [text "Step"]
        , button [onClick ToggleAutomatic] [text automaticText]
        , button [onClick ClearCells] [text "Clear"]
        , slider model
        , div [] [text (toString model.generation)]
        , div [] rows
        ]

slider : Model -> Html Msg
slider {tickTime} =
  let
      timeText =
        tickTime
        |> Time.inSeconds
        |> toString

      attributes = 
        [ type_ "range"
        , value timeText
        , Html.Attributes.min "0.1"
        , Html.Attributes.max "2"
        , step "0.1"
        , onInput ChangeTickTime
        ]
  in
      div []
        [ input attributes []
        , text timeText
        ]

type GridCell = AliveCell Int Int | DeadCell Int Int

createRows : List (List GridCell) -> List (Html Msg)
createRows grid =
  List.map (createColumns) grid

createColumns : List GridCell -> Html Msg
createColumns row =
  let
      columns = List.map (createColumn) row
      styles = style [("display", "inline-block")]
  in
      div [class "row", styles] columns

createColumn : GridCell -> Html Msg
createColumn cell = 
  let
      commonStyles =
        [ ("width", "10px")
        , ("height", "10px")
        , ("border", "1px solid gray")
        ]
      aliveStyle = style (commonStyles ++ [("background-color", "black")])
      deadStyle = style (commonStyles ++ [("brackground-color", "white")])
      (x, y, cellStyle) = case cell of
        AliveCell x y -> (x, y, aliveStyle)
        DeadCell x y -> (x, y, deadStyle)
  in
      div [onClick (ToggleCell x y), cellStyle] []

grid : Model -> List (List GridCell)
grid {gridDimensions, cells} =
  let
      (minX, maxX, minY, maxY) = gridDimensions
      rows = List.range minY maxY
      columns = List.range minX maxX
  in
      List.map (\y -> List.map (\x -> createGridCell (x, y) cells) columns) rows

type alias GridDimensions = (Int, Int, Int, Int)

dimensions : List Cell -> GridDimensions -> GridDimensions
dimensions cells (oldMinX, oldMaxX, oldMinY, oldMaxY) =
  let
      (xs, ys) = List.unzip cells
      padding = 4
      maxX = adjustPadding xs 10 oldMaxX padding
      maxY = adjustPadding ys 10 oldMaxY padding
      minX = adjustPadding xs 0 oldMinX (negate padding)
      minY = adjustPadding ys 0 oldMinY (negate padding)
  in
      ( minX, maxX, minY, maxY )

adjustPadding : List Int -> Int -> Int -> Int -> Int
adjustPadding xs default old padding =
  let
      selectionCriteria = if padding >= 0 then List.maximum else List.minimum
      comparisonWith = if padding >= 0 then Basics.max else Basics.min
  in
      xs 
        |> selectionCriteria
        |> Maybe.withDefault default
        |> snapPadding padding
        |> comparisonWith old

snapPadding : Int -> Int -> Int
snapPadding step value =
  value // (abs step) * (abs step) + step
      
createGridCell : Cell -> List Cell -> GridCell
createGridCell ((x, y) as cell) aliveCells =
  if List.member cell aliveCells then
    AliveCell x y
  else
    DeadCell x y
