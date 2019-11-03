module Main exposing (main)

import Array exposing (..)
import Browser exposing (document)
import Browser.Events exposing ( onKeyDown, onKeyUp)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text as Text
import Color exposing (..)
import Html exposing (Html)
import Html.Attributes as Atts
import Html.Events exposing ( onClick, onInput )
import Json.Decode as Decode
import Time


type alias Flags = {}

type alias Game =
    { board : Board
    , editor : Editor
    , programmer : Programmer
    , executor : Executor
    , mode : Mode
    }

type alias Board =
    { width : Int
    , height : Int
    , player : Player
    , cells : Array Cell
    }

type alias Editor =
    { drawStyle : Boundary
    }

type alias Editing a =
    { a
    | board : Board
    , editor : Editor
    }

type alias Programmer =
    { moves : List Msg
    , program : String
    }


type alias Programming a =
    { a
    | board : Board
    , programmer : Programmer
    }

type alias Executor =
    { commands : List Msg
    }

type alias Executing a =
    { a
    | board : Board
    , executor : Executor
    }


type Mode
    = Edit
    | Program
    | Execute

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
    | Alley

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
    | KeyShift Bool
    | SwitchMode Mode
    | KeyOtherDown String
    | KeyOtherUp String
    | Tick Time.Posix
    | SetPlayer Player
    | ProgramChanged String

testBoard : Board -> Board
testBoard board =
    board
      |> updateCellBoundary (0, 0) Up Alley
      |> updateCellBoundary (0, 1) Up Alley
      |> updateCellBoundary (0, 2) Up Alley
      |> updateCellBoundary (0, 3) Right Alley
      |> updateCellBoundary (1, 3) Right Alley
      |> updateCellBoundary (2, 3) Right Alley
      |> updateCellBoundary (3, 3) Down Alley
      |> updateCellBoundary (3, 2) Down Alley
      |> updateCellBoundary (3, 1) Left Alley
      |> updateCellBoundary (2, 1) Left Alley
      |> updateCellBoundary (1, 1) Left Alley

testProgram : String
testProgram = String.join "\n"
  [ "forward"
  , "right"
  , "right"
  ]

initGame : Flags -> ( Game, Cmd msg )
initGame flags =
    let
        game =
            { board = newBoard 10 10
                |> updateCell ( 0, 0 ) (updateCellType Start)
                |> updateCell ( 9, 9 ) (updateCellType Goal)
                |> testBoard
                |> playerAtStart
            , editor =
                { drawStyle = Alley
                }
            , programmer =
                { moves = []
                , program = testProgram
                }
            , executor =
                { commands = []
                }
            , mode = Edit
            }
    in
        ( game, Cmd.none )

newBoard : Int -> Int -> Board
newBoard width height =
    { width = width
    , height = height
    , cells =
        Array.repeat (width * height)
            { cellType = Empty
            , top = Wall
            , left = Wall
            , bottom = Wall
            , right = Wall
            }
    , player = Player 0 0 Up
    }

updateGame : Msg -> Game -> ( Game, Cmd Msg )
updateGame msg game =
    let
        { board, executor, programmer } = game

        atStart = playerAtStart board

        updatePlayer mode = if mode == Edit
            then board
            else atStart

        updateExecutor mode = if mode == Execute
            then { executor | commands = SetPlayer atStart.player :: programmer.moves }
            else executor

        updatedGame =
            case msg of
                ProgramChanged newProgram ->
                  { game
                  | programmer = { programmer | program = newProgram }
                  }

                SwitchMode mode ->
                    { game
                    | mode = mode
                    , board = updatePlayer mode
                    , executor = updateExecutor mode
                    }

                _ ->
                    case game.mode of
                        Program -> updateGameProgramMode msg game
                        Edit    -> updateGameEditMode msg game
                        Execute -> updateGameExecuteMode msg game
    in
        ( updatedGame, Cmd.none )

updateGameExecuteMode : Msg -> Executing Game -> Executing Game
updateGameExecuteMode msg game =
    let
        { board, executor } = game
        { player } = board
        { orientation } = player
        { commands } = executor

        cycle list = case list of
            []       -> []
            x :: xs  -> xs ++ [ x ]

        movedPlayer =
            case List.head commands of
                Just (SetPlayer p)        -> p
                Just (KeyArrow Up)        -> movePlayer orientation player
                Just (KeyArrow direction) -> turnPlayer direction player
                _                         -> player
    in
        case msg of
          Tick _ -> { game
                    | board = { board | player = movedPlayer }
                    , executor = { executor | commands = cycle commands }
                    }

          _      -> game

updateGameProgramMode : Msg -> Programming Game -> Programming Game
updateGameProgramMode msg game =
    let
        { board, programmer } = game
        { player } = board
        { x, y, orientation } = player
        { moves } = programmer

        blocked direction cell =
            case direction of
                Right -> cell.right == Wall
                Left  -> cell.left == Wall
                Up    -> cell.top == Wall
                Down  -> cell.bottom == Wall

        isBlocked direction =
            queryCell ( x, y ) board (blocked direction)
    in
        case msg of
            KeyArrow Up ->
                if isBlocked orientation then
                    game
                else
                    { game
                    | board = { board | player = movePlayer orientation player }
                    , programmer = { programmer | moves = moves ++ [ msg ] }
                    }

            KeyArrow Down -> game

            KeyArrow direction ->
                { game
                | board = { board | player = turnPlayer direction player }
                , programmer = { programmer | moves = moves ++ [ msg ] }
                }

            _ -> game

updateGameEditMode : Msg -> Editing Game -> Editing Game
updateGameEditMode msg game =
    let
        { board, editor } = game
        { width, height, player } = board
        { x, y } = player
        { drawStyle } = editor

        offBoard direction =
            case direction of
                Right -> x + 1 >= width
                Left  -> x <= 0
                Up    -> y + 1 >= height
                Down  -> y <= 0

        removeWall direction = updateCellBoundary ( x, y ) direction Alley
        restoreWall direction = updateCellBoundary ( x, y ) direction Wall
    in
        case msg of
            KeyShift True    ->
                { game | editor = { editor | drawStyle = Wall }}

            KeyShift False   ->
                { game | editor = { editor | drawStyle = Alley }}

            KeyArrow direction ->
                let
                    updatedBoard =
                        if drawStyle == Wall then
                            { board | player = movePlayer direction player }
                                |> restoreWall Up
                                |> restoreWall Down
                                |> restoreWall Left
                                |> restoreWall Right

                        else { board | player = movePlayer direction player }
                                |> removeWall direction
                in
                    { game | board =
                        if offBoard direction
                            then board
                            else updatedBoard
                    }

            _ ->
                game

playerAtStart : Board -> Board
playerAtStart board =
    let
        player = cellsWithIndex board
            |> List.filter (\(_,_,c) -> c.cellType == Start)
            |> List.head
            |> Maybe.map (\(x,y,_) -> Player x y Up)
            |> Maybe.withDefault (Player 0 0 Up)
    in
        { board | player = player }

movePlayer : Direction -> Player -> Player
movePlayer direction player =
    case direction of
        Right -> { player | x = player.x + 1 }
        Left  -> { player | x = player.x - 1 }
        Up    -> { player | y = player.y + 1 }
        Down  -> { player | y = player.y - 1 }

turnPlayer : Direction -> Player -> Player
turnPlayer direction player =
    { player | orientation =
        case direction of
            Right -> rightOfDirection player.orientation
            Left  -> leftOfDirection player.orientation
            _     -> player.orientation
    }

queryCell : ( Int, Int ) -> Board -> (Cell -> Bool) -> Bool
queryCell ( x, y ) { width, height, cells } query =
    Array.get ((height - 1 - y) * width + x) cells
        |> Maybe.map query
        |> Maybe.withDefault True

updateCell : ( Int, Int ) -> (Cell -> Cell) -> Board -> Board
updateCell ( x, y ) f board =
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

updateCellBoundary : ( Int, Int ) -> Direction -> Boundary -> Board -> Board
updateCellBoundary ( x, y ) direction boundary board =
    let
        update dir cell =
            case dir of
                Up    -> { cell | top = boundary }
                Down  -> { cell | bottom = boundary }
                Left  -> { cell | left = boundary }
                Right -> { cell | right = boundary }

        neighbour =
            case direction of
                Up    -> ( x, y + 1 )
                Down  -> ( x, y - 1 )
                Left  -> ( x - 1, y )
                Right -> ( x + 1, y )
    in
    board
        |> updateCell ( x, y ) (update direction)
        |> updateCell neighbour (update <| oppositeDirection direction)

viewGame : Game -> List (Html Msg)
viewGame game =
    let
        { board } = game
        { player } = board

        playerAt ( x, y ) =
            if (player.x, player.y) == (x, y)
                then Just player.orientation
                else Nothing

        render ( x, y, cell ) = viewCell ( x, y ) (playerAt ( x, y )) cell
    in
        [ cellsWithIndex board
            |> List.map render
            |> group
            |> svg
        , Html.div []
            [ Html.button
                [ onClick <| SwitchMode Edit ]
                [ Html.text "edit" ]
            , Html.button
                [ onClick <| SwitchMode Program ]
                [ Html.text "program" ]
            , Html.button
                [ onClick <| SwitchMode Execute ]
                [ Html.text "execute" ]
            ]
        , Html.div []
            [ Html.textarea
                [ Atts.cols 60
                , Atts.rows 30
                , onInput ProgramChanged
                ]
                [ Html.text game.programmer.program ]
            ]
        ]

oppositeDirection : Direction -> Direction
oppositeDirection d =
    case d of
        Up    -> Down
        Down  -> Up
        Left  -> Right
        Right -> Left

rightOfDirection : Direction -> Direction
rightOfDirection d =
    case d of
        Right -> Down
        Up    -> Right
        Left  -> Up
        Down  -> Left

leftOfDirection : Direction -> Direction
leftOfDirection d =
    case d of
        Right -> Up
        Up    -> Left
        Left  -> Down
        Down  -> Right

cellsWithIndex : Board -> List ( Int, Int, Cell )
cellsWithIndex { width, height, cells } =
    cells
        |> Array.indexedMap
            (\i c ->
                ( modBy width i, height - 1 - i // width, c )
            )
        |> Array.toList


viewCell : ( Int, Int ) -> Maybe Direction -> Cell -> Collage Msg
viewCell ( x, y ) direction { cellType, left, top, bottom, right } =
    let
        wallStyle wall =
            case wall of
                Wall  -> solid thin (uniform black)
                Alley -> invisible

        cell =
            case cellType of
                Empty -> []

                Start ->
                    [ Text.fromString "S"
                        |> Text.color red
                        |> Text.weight Text.Bold
                        |> Text.size Text.huge
                        |> rendered
                    ]

                Goal ->
                    [ Text.fromString "G"
                        |> Text.color green
                        |> Text.weight Text.Bold
                        |> Text.size Text.huge
                        |> rendered
                    ]

        angle d =
            case d of
                Left  -> -pi / 2
                Up    -> pi
                Right -> pi / 2
                Down  -> 0

        player =
            case direction of
                Nothing -> []

                Just d ->
                    [ Text.fromString "T"
                        |> Text.color blue
                        |> Text.weight Text.Bold
                        |> Text.size Text.huge
                        |> rendered
                        |> rotate (angle d)
                    ]
    in
        group
            [ line 50
                |> traced (wallStyle bottom)
                |> shiftY -25
            , line 50
                |> traced (wallStyle top)
                |> shiftY 25
            , line 50
                |> traced (wallStyle right)
                |> rotate (pi / 2)
                |> shiftX 25
            , line 50
                |> traced (wallStyle left)
                |> rotate (pi / 2)
                |> shiftX -25
            , group
                player
            , group
                cell
            , square 50
                |> filled (uniform lightYellow)
            ]
            |> shift ( 50 * toFloat x, 50 * toFloat y )

keyDownDecoder : Decode.Decoder Msg
keyDownDecoder =
    let
        toDirection string =
            case string of
                "ArrowLeft"  -> KeyArrow Left
                "ArrowRight" -> KeyArrow Right
                "ArrowUp"    -> KeyArrow Up
                "ArrowDown"  -> KeyArrow Down
                "Shift"      -> KeyShift True
                "E"          -> SwitchMode Edit
                "e"          -> SwitchMode Edit
                "P"          -> SwitchMode Program
                "p"          -> SwitchMode Program
                "R"          -> SwitchMode Execute
                "r"          -> SwitchMode Execute
                _            -> KeyOtherDown string
    in
        Decode.field "key" Decode.string
            |> Decode.map toDirection

keyUpDecoder : Decode.Decoder Msg
keyUpDecoder =
    let
        toDirection string =
            case string of
                "Shift" -> KeyShift False
                _       -> KeyOtherUp string
    in
        Decode.field "key" Decode.string
            |> Decode.map toDirection

main : Program Flags Game Msg
main =
    document
        { subscriptions =
            \_ -> Sub.batch
                    [ onKeyDown keyDownDecoder
                    , onKeyUp keyUpDecoder
                    , Time.every 400 Tick
                    ]
        , init = initGame
        , update = updateGame
        , view =
            \game ->
                { title = "ElMaze"
                , body = viewGame game
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
