module Main exposing ( main )

import Array exposing (..)
import Browser exposing ( document, Document )
import Browser.Events as Events
import Json.Decode as Decode
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

type Msg
  = KeyRight
  | KeyUp
  | KeyLeft
  | KeyDown
  | KeyOther String

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

debugKey : String -> String
debugKey s =
  let
      on = False
  in
    if on
      then Debug.log s s
      else s

initBoard : Flags -> ( Board, Cmd msg )
initBoard flags =
  ( newBoard 10 10
      |> updateCell (0, 0) ( updateCellType Start )
      |> updateCell (9, 9) ( updateCellType Goal )
  , Cmd.none
  )

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
      ( Player 5 5 Up )
      ( Array.repeat (width * height) cell )

updateBoard : Msg -> Board -> ( Board, Cmd Msg )
updateBoard msg board =
  let
      moveRight player = { player | x = min (board.width - 1) (player.x + 1) }
      moveUp    player = { player | y = min (board.height - 1) (player.y + 1) }
      moveLeft  player = { player | x = max 0 (player.x - 1) }
      moveDown  player = { player | y = max 0 (player.y - 1) }
  in
    case msg of
        KeyRight ->
          ( { board
                | player = moveRight board.player }
                |> updateCellBoundary (board.player.x, board.player.y) Right None
          , Cmd.none
          )
        KeyUp    ->
          ( { board
                | player = moveUp    board.player }
                |> updateCellBoundary (board.player.x, board.player.y) Up None
          , Cmd.none
          )
        KeyLeft  ->
          ( { board
                | player = moveLeft  board.player }
                |> updateCellBoundary (board.player.x, board.player.y) Left None
          , Cmd.none
          )
        KeyDown  ->
          ( { board
                | player = moveDown  board.player }
                |> updateCellBoundary (board.player.x, board.player.y) Down None
          , Cmd.none
          )
        _        -> ( board, Cmd.none )

updateCell : (Int, Int) -> ( Cell -> Cell ) -> Board -> Board
updateCell (x, y) f board =
  let
    { width, height, player, cells } = board
    i = (height - 1 - y) * width + x
    update cell = Board width height player ( Array.set i cell cells )
  in
    Array.get i cells
      |> Maybe.map (f >> update)
      |> Maybe.withDefault board

updateCellType : CellType -> Cell -> Cell
updateCellType t c = { c | cellType = t }

updateCellBoundary : (Int, Int) -> Direction -> Boundary -> Board -> Board
updateCellBoundary (x, y) direction boundary board =
  let
    update dir cell =
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
            ( update direction )

      |> updateCell neighbour
            ( update opposite )

viewBoard : Board -> List ( Html Msg )
viewBoard board =
  let
      playerAt (x, y) =
        if board.player.x == x && board.player.y == y
          then Just board.player.orientation
          else Nothing
      render ( x, y, cell ) = viewCell (x, y) (playerAt (x, y)) cell
  in
    [ cellsWithIndex board
        |> List.map render
        |> group
        |> svg
    ]

cellsWithIndex : Board -> List ( Int, Int, Cell )
cellsWithIndex { width, height, cells } =
  cells
    |> Array.indexedMap (\i c ->
      ( modBy width i, height - 1 - i // width, c ) )
    |> Array.toList

viewCell : ( Int, Int ) -> Maybe Direction -> Cell -> Collage Msg
viewCell (x, y) direction { cellType, left, top, bottom, right } =
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
                    |> rendered
                  ]
        Goal  -> [ Text.fromString "G"
                    |> Text.color green
                    |> Text.weight Text.Bold
                    |> Text.size Text.huge
                    |> rendered
                  ]

      angle d = case d of
          Left  -> pi/2
          Up    -> pi
          Right -> -pi/2
          Down  -> 0

      player = case direction of
         Nothing -> [ ]
         Just d  -> [ Text.fromString "T"
                      |> Text.color blue
                      |> Text.weight Text.Bold
                      |> Text.size Text.huge
                      |> rendered
                      |> rotate (angle d)
                    ]
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
          player
      , group
          cell
      , group
          ( debugGridCoordinates x y )
      , square 50
          |> filled ( uniform lightYellow )
      ]
      |> shift (50 * toFloat x, 50 * toFloat y)

keyDecoder : Decode.Decoder Msg
keyDecoder =
  let
    toDirection string =
      case debugKey string of
        "ArrowLeft"  -> KeyLeft
        "ArrowRight" -> KeyRight
        "ArrowUp"    -> KeyUp
        "ArrowDown"  -> KeyDown
        _            -> KeyOther string
  in
    Decode.field "key" Decode.string
      |> Decode.map toDirection

main : Program Flags Board Msg
main = document
  { subscriptions = \_ -> Sub.batch
      [ Events.onKeyDown keyDecoder ]
  , init = initBoard
  , update = updateBoard
  , view = \board ->
      { title = "ElMaze"
      , body = viewBoard board
      }
  }


-- program
--   = command*
--     WHERE
--     definition*
--
-- definition
--   = commandDefinition
--   | conditionDefinition
--
-- commandDefinition
--   = COMMAND_IDENTIFIER IDENTIFIER* ASSIGN ...
--
-- conditionDefinition
--   = CONDITION_IDENTIFIER IDENTIFIER* ASSIGN ...
--
-- command
--   = COMMAND_IDENTIFIER parameter*
--   | CONDITION_IDENTIFIER parameter* command
--
-- parameter
--   = NUM
--   | BOOL
--
-- COMMAND_IDENTIFIER = IDENTIFIER
-- CONDITION_IDENTIFIER = IDENTIFIER"?"
-- IDENTIFIER = [a-zA-Z][a-zA-Z0-9]*
-- ASSIGN = "="
-- NUM = [+-]?0*[0-9]
-- BOOL = "true"|"false"
