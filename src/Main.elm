module Main exposing (..)

import Array exposing (..)

import Browser exposing ( document, Document )
import Collage exposing (..)
import Collage.Text as Text
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Html exposing ( Html )

type alias Flags = { }

type Board = Board Int Int (Array Cell)

type alias Cell =
  { x : Int
  , y : Int
  , cellType : CellType
  , top : Boundary
  , left : Boundary
  , bottom : Boundary
  , right : Boundary
  }

type CellType
  = Empty
  | Start
  | Goal

type Boundary
  = Wall
  | None

type Player = Player Int Int

type Direction
  = Right
  | Up
  | Left
  | Down

type alias Model =
  ( Board
  , Player
  )

type Msg
  = Switch
  | Tick

subscriptions : Model -> Sub msg
subscriptions model = Sub.none

newBoard : Int -> Int -> Board
newBoard width height =
  let
    cell =
      { x = 0
      , y = 0
      , cellType = Empty
      , top = Wall
      , left = Wall
      , bottom = Wall
      , right = Wall
      }
  in
    Board width height
      ( Array.repeat (width * height) cell
        |> Array.indexedMap ( \i c ->
          { c
          | x = modBy width i
          , y = height - 1 - i // width
          } )
      )

updateCellBoundary : Direction -> Boundary -> Cell -> Cell
updateCellBoundary direction boundary cell =
  case direction of
    Up    -> { cell | top = boundary }
    Down  -> { cell | bottom = boundary }
    Left  -> { cell | left = boundary }
    Right -> { cell | right = boundary }

opposite : Direction -> Direction
opposite direction =
  case direction of
    Up    -> Down
    Down  -> Up
    Left  -> Right
    Right -> Left

setCellBoundary : (Int, Int) -> Direction -> Boundary -> Board -> Board
setCellBoundary (x, y) direction boundary board =
  let
    cell      = board
                  |> getCell (x, y)
                  |> Maybe.map ( updateCellBoundary direction boundary )
    neighbour = board
                  |> getNeighbour (x, y) direction
                  |> Maybe.map ( updateCellBoundary (opposite direction) boundary )
  in
    Just board
      |> Maybe.map2 setCell cell
      |> Maybe.map2 setCell neighbour
      |> Maybe.withDefault board

setCell : Cell -> Board -> Board
setCell cell board =
  let
    ( Board width height cells ) = board
    i = (height - 1 - cell.y) * width + cell.x
  in
    if isOnBoard (cell.x, cell.y) board
      then Board width height ( Array.set i cell cells )
      else board

getCell : (Int, Int) -> Board -> Maybe Cell
getCell (x, y) board =
  let
    ( Board width height cells ) = board
    i = (height - 1 - y) * width + x
  in
    if isOnBoard (x, y) board
      then Array.get i cells
      else Nothing

getNeighbour : (Int, Int) -> Direction -> Board -> Maybe Cell
getNeighbour (x, y) direction board =
  case direction of
    Up    -> getCell ( x, y + 1 ) board
    Down  -> getCell ( x, y - 1 ) board
    Left  -> getCell ( x - 1, y ) board
    Right -> getCell ( x + 1, y ) board

isOnBoard : (Int, Int) -> Board -> Bool
isOnBoard (x, y) (Board width height _) =
  x >= 0 && x < width &&
  y >= 0 && y < height

initBoard : Board
initBoard =
  newBoard 10 10
    |> setCellBoundary (0, 0) Right None
    |> setCellBoundary (1, 0) Up None
    |> setCellBoundary (1, 1) Right None
    |> setCellBoundary (2, 1) Right None
    |> setCellBoundary (3, 1) Right None
    |> setCellBoundary (4, 1) Up None
    |> setCellBoundary (4, 2) Up None
    |> setCellBoundary (4, 3) Up None
    |> setCellBoundary (4, 4) Left None
    |> setCellBoundary (3, 4) Left None
    |> setCellBoundary (2, 4) Down None
    |> setCellBoundary (2, 3) Down None
    |> setCellBoundary (2, 2) Down None

initPlayer : Player
initPlayer = Player 0 0

init : Flags -> ( Model, Cmd msg )
init flags =
  ( ( initBoard, initPlayer )
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model
  , Cmd.none
  )

renderCell : Cell -> Collage Msg
renderCell cell =
  let
      wallStyle wall = case wall of
        Wall -> solid thin ( uniform black )
        None -> invisible
  in
    group
      [ line 50
          |> traced ( wallStyle cell.bottom )
          |> shiftY -25
      , line 50
          |> traced ( wallStyle cell.top )
          |> shiftY 25
      , line 50
          |> traced ( wallStyle cell.right )
          |> rotate (pi/2)
          |> shiftX 25
      , line 50
          |> traced ( wallStyle cell.left )
          |> rotate (pi/2)
          |> shiftX -25
      , Text.fromString (String.fromInt cell.x ++ "," ++ String.fromInt cell.y)
          |> rendered
      , square 50
          |> filled ( uniform lightYellow )
      ]
      |> shift (50 * toFloat cell.x, 50 * toFloat cell.y)

renderBoard : Board -> List ( Html Msg )
renderBoard (Board width height cells) =
  [ cells
      |> Array.map renderCell
      |> Array.toList
      |> group
      |> svg
  ]

view : Model -> Document Msg
view ( board, player ) =
  { title = "Testi"
  , body = renderBoard board
  }

main : Program Flags Model Msg
main = document
  { subscriptions = subscriptions
  , init = init
  , update = update
  , view = view
  }