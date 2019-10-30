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
import Time

type alias Flags = { }

type alias Board =
  { width : Int
  , height : Int
  , player : Player
  , cells : Array Cell
  , shiftDown : Bool
  , mode : Mode
  , replayLog : List Msg
  }

type Mode = Edit | Play | Replay

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
  = KeyArrow Direction
  | KeyShiftDown
  | KeyShiftUp
  | SwitchMode Mode
  | KeyOtherDown String
  | KeyOtherUp String
  | Tick Time.Posix
  | SetPlayer Player

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
      newPlayer
      ( Array.repeat (width * height) cell )
      False Edit
      [ SetPlayer newPlayer ]

newPlayer : Player
newPlayer = Player 0 0 Up

updateBoard : Msg -> Board -> ( Board, Cmd Msg )
updateBoard msg board =
  let
      updatedBoard = case msg of
        SwitchMode mode -> { board | mode = mode, player = newPlayer }

        _  -> case board.mode of
                Play    ->  updateBoardPlayMode   msg board
                Edit    ->  updateBoardEditMode   msg board
                Replay  ->  updateBoardReplayMode msg board
  in
      ( updatedBoard, Cmd.none )

updateBoardReplayMode : Msg -> Board -> Board
updateBoardReplayMode msg board =
    case msg of
      Tick _ ->
        let
          cycle list = case list of
             []      -> []
             x :: xs -> xs ++ [x]

          cycledLog = cycle board.replayLog
          move = List.head board.replayLog

          turn player direction = { player | orientation = case direction of
              Right -> rightOfDirection player.orientation
              Left  -> leftOfDirection player.orientation
              _     -> player.orientation
            }

          movedPlayer = case move of
            Just (SetPlayer player)   -> player
            Just (KeyArrow Up)        -> movePlayer board.player.orientation board.player
            Just (KeyArrow direction) -> turn board.player direction
            _ -> board.player

          updatedBoard = { board
                            | replayLog = cycledLog
                            , player = movedPlayer
                            }
        in
          updatedBoard

      _ -> board

updateBoardPlayMode : Msg -> Board -> Board
updateBoardPlayMode msg board =
  let
      { player, replayLog } = board
      { x, y, orientation } = player

      turn direction = { player | orientation = case direction of
          Right -> rightOfDirection orientation
          Left  -> leftOfDirection orientation
          _     -> orientation
        }

      blocked direction cell = case direction of
         Right -> cell.right == Wall
         Left  -> cell.left == Wall
         Up    -> cell.top == Wall
         Down  -> cell.bottom == Wall

      isBlocked direction = getCell (x, y) board
          |> Maybe.map (blocked direction)
          |> Maybe.withDefault True
  in
      case msg of
        KeyArrow Up ->
            if isBlocked orientation
              then   board
              else { board
                      | player = movePlayer orientation player
                      , replayLog = replayLog ++ [msg]
                      }

        KeyArrow Down -> board

        KeyArrow direction ->
            { board
                | player = turn direction
                , replayLog = replayLog ++ [msg]
                }

        _    -> board

updateBoardEditMode : Msg -> Board -> Board
updateBoardEditMode msg board =
  let
      { width, height, player, shiftDown } = board
      { x, y }   = player

      offBoard direction = case direction of
        Right -> x + 1 >= width
        Left  -> x     <= 0
        Up    -> y + 1 >= height
        Down  -> y     <= 0

      removeWall direction  = updateCellBoundary (x, y) direction None
      restoreWall direction = updateCellBoundary (x, y) direction Wall
  in
      case msg of
          KeyShiftDown    -> { board | shiftDown = True }
          KeyShiftUp      -> { board | shiftDown = False }

          KeyArrow direction ->
            if offBoard direction
              then   board
              else if shiftDown then
                  { board
                      | player = movePlayer direction player }
                      |> restoreWall Up
                      |> restoreWall Down
                      |> restoreWall Left
                      |> restoreWall Right
              else
                  { board
                      | player = movePlayer direction player }
                      |> removeWall direction

          _ -> board

movePlayer : Direction -> Player -> Player
movePlayer direction player = case direction of
    Right -> { player | x = player.x + 1 }
    Left  -> { player | x = player.x - 1 }
    Up    -> { player | y = player.y + 1 }
    Down  -> { player | y = player.y - 1 }

getCell : (Int, Int) -> Board -> Maybe Cell
getCell (x, y) { width, height, cells } =
    Array.get ((height - 1 - y) * width + x) cells

updateCell : (Int, Int) -> ( Cell -> Cell ) -> Board -> Board
updateCell (x, y) f board =
  let
    { width, height, cells } = board
    i = (height - 1 - y) * width + x
    update cell = { board | cells = Array.set i cell cells }
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
  in
    board
      |> updateCell (x, y)
            ( update direction )

      |> updateCell neighbour
            ( update <| oppositeDirection direction )

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

oppositeDirection : Direction -> Direction
oppositeDirection d =
  case d of
    Up    -> Down
    Down  -> Up
    Left  -> Right
    Right -> Left

rightOfDirection : Direction -> Direction
rightOfDirection d = case d of
    Right -> Down
    Up -> Right
    Left -> Up
    Down -> Left

leftOfDirection : Direction -> Direction
leftOfDirection d = case d of
    Right -> Up
    Up -> Left
    Left -> Down
    Down -> Right

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
          Left  -> -pi/2
          Up    -> pi
          Right -> pi/2
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
      , square 50
          |> filled ( uniform lightYellow )
      ]
      |> shift (50 * toFloat x, 50 * toFloat y)

keyDownDecoder : Decode.Decoder Msg
keyDownDecoder =
  let
    toDirection string =
      case string of
        "ArrowLeft"  -> KeyArrow Left
        "ArrowRight" -> KeyArrow Right
        "ArrowUp"    -> KeyArrow Up
        "ArrowDown"  -> KeyArrow Down
        "Shift"      -> KeyShiftDown
        "E"          -> SwitchMode Edit
        "e"          -> SwitchMode Edit
        "P"          -> SwitchMode Play
        "p"          -> SwitchMode Play
        "R"          -> SwitchMode Replay
        "r"          -> SwitchMode Replay
        _            -> KeyOtherDown string
  in
    Decode.field "key" Decode.string
      |> Decode.map toDirection

keyUpDecoder : Decode.Decoder Msg
keyUpDecoder =
  let
    toDirection string =
      case string of
        "Shift"      -> KeyShiftUp
        _            -> KeyOtherUp string
  in
    Decode.field "key" Decode.string
      |> Decode.map toDirection

main : Program Flags Board Msg
main = document
  { subscriptions = \_ -> Sub.batch
      [ Events.onKeyDown keyDownDecoder
      , Events.onKeyUp   keyUpDecoder
      , Time.every 400 Tick
      ]
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
