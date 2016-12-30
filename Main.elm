import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import GameOfLife exposing (nextGeneration, Cell)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg = NextGeneration

init = [(0, 1), (1, 1), (2, 1)] ! []

update : Msg -> List Cell -> (List Cell, Cmd Msg)
update msg model =
  case msg of
    NextGeneration -> (nextGeneration model, Cmd.none)

subscriptions model =
  Sub.none

type GridCell = AliveCell Int Int | DeadCell Int Int

view : List Cell -> Html Msg
view model =
  let
      cellGrid = grid model
      rows = createRows cellGrid
  in
      div []
        [ div [] [text (toString model)]
        , button [onClick NextGeneration] [text "Step"]
        , div [] rows
        ]

createRows : List (List GridCell) -> List (Html msg)
createRows grid =
  List.map (createColumns) grid

createColumns : List GridCell -> Html msg
createColumns row =
  let
      columns = List.map (createColumn) row
      styles = style [("display", "inline-block")]
  in
      div [class "row", styles] columns

createColumn : GridCell -> Html msg
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
      div [cellStyle] []

grid : List Cell -> List (List GridCell)
grid model =
  let
      (minX, maxX, minY, maxY) = dimensions model
      rows = List.range minY maxY
      columns = List.range minX maxX
  in
      List.map (\y -> List.map (\x -> createGridCell (x, y) model) columns) rows

type alias GridDimensions = (Int, Int, Int, Int)

dimensions : List Cell -> GridDimensions
dimensions model =
  let
      padding = 5
      (xs, ys) = List.unzip model
      maxX = (Maybe.withDefault 10 << List.maximum) xs
      minX = (Maybe.withDefault 10 << List.minimum) xs
      maxY = (Maybe.withDefault 10 << List.maximum) ys
      minY = (Maybe.withDefault 10 << List.minimum) ys
  in
      (-10, maxX + padding, -10, maxY + padding)
      
createGridCell : Cell -> List Cell -> GridCell
createGridCell cell aliveCells =
  let
      (x, y) = cell
  in
      if List.member cell aliveCells then
        AliveCell x y
      else
        DeadCell x y
