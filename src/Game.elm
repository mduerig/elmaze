module Game exposing (..)

import Array exposing ( Array )
import Browser
import Browser.Events exposing ( onKeyDown, onKeyUp)
import Collage exposing ( .. )
import Collage.Render exposing ( svg )
import Collage.Text as Text
import Color
import Html exposing ( Html )
import Html.Attributes as Atts
import Html.Events exposing ( onClick, onInput )
import Json.Decode as Decode
import Time


type alias Game solver =
    { board : Board
    , mode : Mode
    , editor : Editor
    , programmer : Programmer
    , executor : Executor solver
    }

type alias Configuration solver =
  { board : Board
  , init : Solve -> solver
  , update : solver -> ( solver, Move )
  }

type Move
    = Forward
    | TurnLeft
    | TurnRight
    | Nop

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

type alias Executor solver =
    { solver : Maybe solver
    , init : Solve -> solver
    , update : solver -> ( solver, Move )
    }

type alias Executing a solver =
    { a
    | board : Board
    , executor : Executor solver
    }

type alias Solve =
    { board : Board
    , program : String
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
    | ProgramChanged String

testProgram : String
testProgram = String.join "\n"
  [ "forward"
  , "right"
  , "right"
  ]

initGame : Configuration solver -> ( Game solver, Cmd msg )
initGame { board, init, update } =
    let
        game =
            { board = board
                |> playerAtStart
            , mode = Edit
            , editor =
                { drawStyle = Alley
                }
            , programmer =
                { moves = []
                , program = testProgram
                }
            , executor =
                { solver = Nothing
                , init = init
                , update = update
                }
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

updateGame : Msg -> Game solver -> ( Game solver, Cmd Msg )
updateGame msg game =
    let
        { board, programmer, executor } = game

        updatePlayer mode = if mode == Edit
            then board
            else playerAtStart board

        updateExecutor mode = if mode == Execute
            then { executor
                 | solver = Just
                     <| executor.init
                     <| Solve board programmer.program
                 }
            else { executor | solver = Nothing }

        updateProgrammer mode = if mode == Program
            then { programmer | moves = [] }
            else programmer

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
                    , programmer = updateProgrammer mode
                    }

                _ ->
                    case game.mode of
                        Program -> updateGameProgramMode msg game
                        Edit    -> updateGameEditMode msg game
                        Execute -> updateGameExecuteMode msg game
    in
        ( updatedGame, Cmd.none )

updateGameExecuteMode : Msg -> Executing a s -> Executing a s
updateGameExecuteMode msg game =
    let
        { board, executor } = game
        { player } = board

        ( updatedSolver, move ) = case executor.solver of
           Just solver -> executor.update solver
                            |> Tuple.mapFirst Just
           _           -> ( Nothing, Nop )

        movedPlayer = player |> case move of
            Forward   -> movePlayer player.orientation
            TurnLeft  -> turnPlayer Left
            TurnRight -> turnPlayer Right
            _         -> always identity
    in
        case msg of
          Tick _ -> { game
                    | board = movedPlayer board
                    , executor = { executor | solver = updatedSolver }
                    }
          _      -> game

updateGameProgramMode : Msg -> Programming a -> Programming a
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
            KeyArrow Down -> game

            KeyArrow Up ->
                if isBlocked orientation then
                    game
                else
                    { game
                    | board = movePlayer orientation player board
                    , programmer = { programmer | moves = moves ++ [ msg ] }
                    }

            KeyArrow direction ->
                { game
                | board = turnPlayer direction player board
                , programmer = { programmer | moves = moves ++ [ msg ] }
                }

            _ -> game

updateGameEditMode : Msg -> Editing a -> Editing a
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
                            board
                                |> movePlayer direction player
                                |> restoreWall Up
                                |> restoreWall Down
                                |> restoreWall Left
                                |> restoreWall Right

                        else board
                                |> movePlayer direction player
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
        startCellToPlayer (x, y, cell) = if cell.cellType == Start
            then Just (Player x y Up)
            else Nothing

        player = cellsWithIndex board
            |> List.filterMap startCellToPlayer
            |> List.head
            |> Maybe.withDefault (Player 0 0 Up)
    in
        { board | player = player }

movePlayer : Direction -> Player -> Board -> Board
movePlayer direction player board =
    let
        movedPlayer = case direction of
            Right -> { player | x = player.x + 1 }
            Left  -> { player | x = player.x - 1 }
            Up    -> { player | y = player.y + 1 }
            Down  -> { player | y = player.y - 1 }
    in
        { board | player = movedPlayer }

turnPlayer : Direction -> Player -> Board -> Board
turnPlayer direction player board =
    let
        turnedPlayer = { player | orientation =
            case direction of
                Right -> rightOfDirection player.orientation
                Left  -> leftOfDirection player.orientation
                _     -> player.orientation
            }
    in
        { board | player = turnedPlayer }

queryCell : ( Int, Int ) -> Board -> (Cell -> Bool) -> Bool
queryCell ( x, y ) { width, height, cells } query =
    Array.get ((height - 1 - y) * width + x) cells
        |> Maybe.map query
        |> Maybe.withDefault True

updateCell : ( Int, Int ) -> (Cell -> Cell) -> Board -> Board
updateCell ( x, y ) update board =
    let
        { width, height, cells } = board
        i = (height - 1 - y) * width + x
        set cell = { board | cells = Array.set i cell cells }
    in
        Array.get i cells
            |> Maybe.map (update >> set)
            |> Maybe.withDefault board

updateCellType : CellType -> Cell -> Cell
updateCellType cellType cell = { cell | cellType = cellType }

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

viewGame : Game solver -> List (Html Msg)
viewGame game =
    let
        { board, programmer } = game
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
                [ Atts.cols 40
                , Atts.rows 30
                , Atts.value <| toProgram programmer.moves
                ]
                [ ]
            , Html.textarea
                [ Atts.cols 40
                , Atts.rows 30
                , onInput ProgramChanged
                ]
                [ Html.text programmer.program ]
            ]
        ]

toProgram : List Msg -> String
toProgram messages =
    let
        toCommand msg = case msg of
           KeyArrow Up    -> "forward"
           KeyArrow Left  -> "left"
           KeyArrow Right -> "right"
           _              -> ""
    in
        messages
            |> List.map toCommand
            |> String.join "\n"

oppositeDirection : Direction -> Direction
oppositeDirection direction =
    case direction of
        Up    -> Down
        Down  -> Up
        Left  -> Right
        Right -> Left

rightOfDirection : Direction -> Direction
rightOfDirection direction =
    case direction of
        Right -> Down
        Up    -> Right
        Left  -> Up
        Down  -> Left

leftOfDirection : Direction -> Direction
leftOfDirection direction =
    case direction of
        Right -> Up
        Up    -> Left
        Left  -> Down
        Down  -> Right

cellsWithIndex : Board -> List ( Int, Int, Cell )
cellsWithIndex { width, height, cells } =
    cells
        |> Array.indexedMap
            (\i cell ->
                ( modBy width i, height - 1 - i // width, cell )
            )
        |> Array.toList


viewCell : ( Int, Int ) -> Maybe Direction -> Cell -> Collage Msg
viewCell ( x, y ) playerOrientation { cellType, left, top, bottom, right } =
    let
        wallStyle wall =
            case wall of
                Wall  -> solid thin (uniform Color.black)
                Alley -> invisible

        cell =
            case cellType of
                Empty -> []

                Start ->
                    [ Text.fromString "S"
                        |> Text.color Color.red
                        |> Text.weight Text.Bold
                        |> Text.size Text.huge
                        |> rendered
                    ]

                Goal ->
                    [ Text.fromString "G"
                        |> Text.color Color.green
                        |> Text.weight Text.Bold
                        |> Text.size Text.huge
                        |> rendered
                    ]

        angle direction =
            case direction of
                Left  -> -pi / 2
                Up    -> pi
                Right -> pi / 2
                Down  -> 0

        player =
            case playerOrientation of
                Nothing -> []

                Just direction ->
                    [ Text.fromString "T"
                        |> Text.color Color.blue
                        |> Text.weight Text.Bold
                        |> Text.size Text.huge
                        |> rendered
                        |> rotate (angle direction)
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
                |> filled (uniform Color.lightYellow)
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

play : Configuration solver -> Program () (Game solver) Msg
play configuration =
    Browser.document
        { subscriptions =
            \_ -> Sub.batch
                    [ onKeyDown keyDownDecoder
                    , onKeyUp keyUpDecoder
                    , Time.every 400 Tick
                    ]
        , init = \_ -> initGame configuration
        , update = updateGame
        , view =
            \game ->
                { title = "ElMaze"
                , body = viewGame game
                }
        }

