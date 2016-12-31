import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import GameOfLife exposing (nextGeneration, Cell)
import Time exposing (Time)
import Keyboard exposing (..)
import Char exposing (..)
import Debug exposing (..)

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

type alias GridDimensions = (Int, Int, Int, Int)

type SliderStep = SliderUp | SliderDown

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

defaultCells : List Cell
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
    , tickTime = sliderConfig.default * Time.second
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
          |> Result.withDefault sliderConfig.default
          |> (*) Time.second
      } ! []
    KeyDown code ->
      let
          keyName = Char.fromCode code
          newModel = handleKeyDown model keyName
      in
          newModel ! []


toggleAutomatic : Model -> Model
toggleAutomatic model =
  { model | automatic = not model.automatic }

handleKeyDown : Model -> Char -> Model
handleKeyDown model keyName =
  case keyName of
    'S' -> advanceGeneration model
    'C' -> clear model
    'X' -> adjustTickTime model SliderUp
    'Z' -> adjustTickTime model SliderDown
    'P' -> toggleAutomatic model
    _ -> model

adjustTickTime : Model -> SliderStep -> Model
adjustTickTime model step =
  let
      minimum = sliderConfig.min * Time.second
      maximum = sliderConfig.max * Time.second
      increment = sliderConfig.step * Time.second
      delta = 
        case step of
          SliderUp -> increment
          SliderDown -> negate increment
  in
      { model |
        tickTime = clamp minimum maximum (model.tickTime + delta)
      }

sliderConfig =
  { min = 0.1
  , max = 2.0
  , step = 0.1
  , default = 1.0
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
      rows = createRows model
      automaticText = if model.automatic then "Stop" else "Start"
  in
      div []
        [ div [] [ strong [] [text <| "Generation: " ++ (toString model.generation)] ]
        , button [onClick NextGeneration] [text "Step"]
        , button [onClick ToggleAutomatic] [text automaticText]
        , button [onClick ClearCells] [text "Clear"]
        , slider model
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
        , Html.Attributes.min (toString sliderConfig.min)
        , Html.Attributes.max (toString sliderConfig.max)
        , step (toString sliderConfig.step)
        , onInput ChangeTickTime
        ]
  in
      div []
        [ input attributes []
        , text timeText
        ]

createRows : Model -> List (Html Msg)
createRows ({gridDimensions, cells} as model) =
  let
      (minX, maxX, minY, maxY) = gridDimensions
      rowRange = List.range minY maxY
      columnRange = List.range minX maxX
  in
      List.map (createRow columnRange cells) rowRange

createRow : List Int -> List Cell -> Int -> Html Msg
createRow columnRange cells y =
  let
      columns = List.map (createCell cells y) columnRange
      styles = style [("display", "inline-block")]
  in
      div [class "row", styles] columns

createCell : List Cell -> Int -> Int -> Html Msg
createCell cells y x = 
  let
      isAlive = List.member (x, y) cells
      commonStyles =
        [ ("width", "10px")
        , ("height", "10px")
        , ("border", "1px solid gray")
        ]
      aliveStyle = style (commonStyles ++ [("background-color", "black")])
      deadStyle = style (commonStyles ++ [("brackground-color", "white")])
      cellStyle = if isAlive then aliveStyle else deadStyle
  in
      div [onClick (ToggleCell x y), cellStyle] []

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
