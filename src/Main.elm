module Main exposing ( main )

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
  { cellType : CellType
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
      { cellType = Empty
      , top = Wall
      , left = Wall
      , bottom = Wall
      , right = Wall
      }
  in
    Board width height
      ( Array.repeat (width * height) cell )

setCellBoundary : (Int, Int) -> Direction -> Boundary -> Board -> Board
setCellBoundary (x, y) direction boundary board =
  let
    updateCellBoundary dir cell =
      case dir of
        Up    -> { cell | top = boundary }
        Down  -> { cell | bottom = boundary }
        Left  -> { cell | left = boundary }
        Right -> { cell | right = boundary }

    neighbour  =
      case direction of
        Up    -> ( x, y + 1 )
        Down  -> ( x, y - 1 )
        Left  -> ( x - 1, y )
        Right -> ( x + 1, y )

    opposite =
      case direction of
        Up    -> Down
        Down  -> Up
        Left  -> Right
        Right -> Left
  in
    board
      |> updateCell (x, y)
            ( updateCellBoundary direction )

      |> updateCell neighbour
            ( updateCellBoundary opposite )

updateCell : (Int, Int) -> ( Cell -> Cell ) -> Board -> Board
updateCell (x, y) f board =
  let
    ( Board width height cells ) = board
    i = (height - 1 - y) * width + x
    updateBoard cell = Board width height ( Array.set i cell cells )
  in
    Array.get i cells
      |> Maybe.map (f >> updateBoard)
      |> Maybe.withDefault board

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

renderCell : ( Int, Int ) -> Cell -> Collage Msg
renderCell (x, y) cell =
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
      , Text.fromString (String.fromInt x ++ "," ++ String.fromInt y)
          |> rendered
      , square 50
          |> filled ( uniform lightYellow )
      ]
      |> shift (50 * toFloat x, 50 * toFloat y)

cellsWithIndex : Board -> List ( Int, Int, Cell )
cellsWithIndex ( Board width height cells ) =
  cells
    |> Array.indexedMap (\i c ->
      ( modBy width i, height - 1 - i // width, c ) )
    |> Array.toList

renderBoard : Board -> List ( Html Msg )
renderBoard board =
  let
      render ( x, y, cell ) = renderCell (x, y) cell
  in
    [ cellsWithIndex board
        |> List.map render
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