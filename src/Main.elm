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

type alias Board =
  { width : Int
  , height : Int
  , player : Player
  , cells : Array Cell
  }

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

type alias Player =
  { x : Int
  , y : Int
  , orientation : Direction
  }

type Direction
  = Right
  | Up
  | Left
  | Down

type alias Model = Board

type Msg
  = Switch
  | Tick

debugGridCoordinates : Int -> Int -> List ( Collage msg )
debugGridCoordinates x y =
  let
      on = False
  in
    if on then
        [ Text.fromString (String.fromInt x ++ "," ++ String.fromInt y)
            |> Text.size Text.tiny
            |> rendered
            |> shift (-12, 12)
        ]
    else []

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
      ( Player 0 0 Up )
      ( Array.repeat (width * height) cell )

setType : CellType -> Cell -> Cell
setType t c = { c | cellType = t }

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
    { width, height, player, cells } = board
    i = (height - 1 - y) * width + x
    updateBoard cell = Board width height player ( Array.set i cell cells )
  in
    Array.get i cells
      |> Maybe.map (f >> updateBoard)
      |> Maybe.withDefault board

initBoard : Board
initBoard =
  newBoard 10 10
    |> updateCell (0, 0) ( setType Start )
    |> updateCell (9, 9) ( setType Goal )
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

init : Flags -> ( Model, Cmd msg )
init flags =
  ( initBoard
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model
  , Cmd.none
  )

renderCell : ( Int, Int ) -> Cell -> Collage Msg
renderCell (x, y) { cellType, left, top, bottom, right } =
  let
      wallStyle wall = case wall of
        Wall -> solid thin ( uniform black )
        None -> invisible

      cell = case cellType of
        Empty -> [ ]
        Start -> [ Text.fromString "S"
                    |> Text.color red
                    |> Text.weight Text.Bold
                    |> Text.size Text.huge
                    |> rendered ]
        Goal  -> [ Text.fromString "G"
                    |> Text.color green
                    |> Text.weight Text.Bold
                    |> Text.size Text.huge
                    |> rendered ]
  in
    group
      [ line 50
          |> traced ( wallStyle bottom )
          |> shiftY -25
      , line 50
          |> traced ( wallStyle top )
          |> shiftY 25
      , line 50
          |> traced ( wallStyle right )
          |> rotate (pi/2)
          |> shiftX 25
      , line 50
          |> traced ( wallStyle left )
          |> rotate (pi/2)
          |> shiftX -25
      , group
          cell
      , group
          ( debugGridCoordinates x y )
      , square 50
          |> filled ( uniform lightYellow )
      ]
      |> shift (50 * toFloat x, 50 * toFloat y)

cellsWithIndex : Board -> List ( Int, Int, Cell )
cellsWithIndex { width, height, cells } =
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
view board =
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