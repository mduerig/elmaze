module Game exposing (..)

import Array exposing ( Array )
import Browser
import Browser.Events exposing ( onKeyDown, onKeyUp, onAnimationFrameDelta )
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
    , animation : Animation
    , editor : Editor
    , programmer : Programmer
    , executor : Executor solver
    }

type alias Animation =
    { v : Float
    , t : Float
    , playerX : Float -> Float
    , playerY : Float -> Float
    , playerOrientation : Float -> Float
    }

type alias Configuration solver =
  { board : Board
  , init : Board -> String -> solver
  , update : Board -> solver -> ( solver, Move )
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
    , tiles : Array Tile
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
    , animation : Animation
    , programmer : Programmer
    }

type alias Executor solver =
    { solver : Maybe solver
    , init : Board -> String -> solver
    , update : Board -> solver -> ( solver, Move )
    }

type alias Executing a solver =
    { a
    | board : Board
    , executor : Executor solver
    }

type Mode
    = Edit
    | Program
    | Execute

type alias Tile =
    { tileType : TileType
    , top : Boundary
    , left : Boundary
    , bottom : Boundary
    , right : Boundary
    }

type TileType
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
    | AnimationFrame Float
    | ProgramChanged String

initGame : Configuration solver -> ( Game solver, Cmd msg )
initGame { board, init, update } =
    let
        game =
            { board = board
                |> playerAtStart
            , mode = Edit
            , animation = { startAnimation | t = 1 }
            , editor =
                { drawStyle = Alley
                }
            , programmer =
                { moves = []
                , program = ""
                }
            , executor =
                { solver = Nothing
                , init = init
                , update = update
                }
            }
    in
        ( game, Cmd.none )

startAnimation : Animation
startAnimation =
    { v = 1.5
    , t = 0
    , playerX = always 0
    , playerY = always 0
    , playerOrientation = always 0
    }

newBoard : Int -> Int -> Board
newBoard width height =
    { width = width
    , height = height
    , tiles =
        Array.repeat (width * height)
            { tileType = Empty
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
        { board, programmer, executor, animation } = game

        updatePlayerPos mode = if mode == Edit
            then board
            else playerAtStart board

        updateExecutor mode = if mode == Execute
            then { executor
                 | solver = Just
                     <| executor.init board programmer.program
                 }
            else { executor | solver = Nothing }

        updateProgrammer mode = if mode == Program
            then { programmer | moves = [] }
            else programmer

        updatedGame =
            case msg of
                AnimationFrame dt ->
                    { game
                    | animation = { animation | t = animation.t + animation.v * dt / 1000 }
                    }

                ProgramChanged newProgram ->
                    { game
                    | programmer = { programmer | program = newProgram }
                    }

                SwitchMode mode ->
                    { game
                    | mode = mode
                    , board = updatePlayerPos mode
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
        { x, y, orientation } = board.player

        ( updatedSolver, move ) = case executor.solver of
           Just solver
                -> executor.update board solver
                    |> Tuple.mapFirst Just
           _
                -> ( Nothing, Nop )

        isFree direction =
            queryTile ( x, y ) board ( hasBoundary direction Alley )

        movedPlayer = case move of
            Forward
                -> if isFree orientation
                    then updatePlayer ( movePlayer orientation )
                    else identity

            TurnLeft
                -> updatePlayer (turnPlayer Left)

            TurnRight
                -> updatePlayer (turnPlayer Right)

            _
                -> identity
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
        { x, y, orientation } = board.player
        { moves } = programmer

        isBlocked direction =
            queryTile ( x, y ) board ( hasBoundary direction Wall )
    in
        case msg of
            KeyArrow Down -> game

            KeyArrow Up ->
                if isBlocked orientation then
                    game
                else
                    { game
                    | board = updatePlayer ( movePlayer orientation ) board
                    , animation = movePlayerAnimation orientation
                    , programmer = { programmer | moves = moves ++ [ msg ] }
                    }

            KeyArrow direction ->
                { game
                | board = updatePlayer ( turnPlayer direction ) board
                , animation = turnPlayerAnimation direction
                , programmer = { programmer | moves = moves ++ [ msg ] }
                }

            _ -> game

movePlayerAnimation : Direction -> Animation
movePlayerAnimation direction =
    case direction of
        Left  -> { startAnimation | playerX = \t -> 1 - t }
        Right -> { startAnimation | playerX = \t -> t - 1 }
        Up    -> { startAnimation | playerY = \t -> t - 1 }
        Down  -> { startAnimation | playerY = \t -> 1 - t }

turnPlayerAnimation : Direction -> Animation
turnPlayerAnimation direction =
    case direction of
        Left  -> { startAnimation | playerOrientation = \t -> pi/2 * (t - 1) }
        Right -> { startAnimation | playerOrientation = \t -> pi/2 * (1 - t) }
        _     ->   startAnimation

updateGameEditMode : Msg -> Editing a -> Editing a
updateGameEditMode msg game =
    let
        { board, editor } = game
        { width, height } = board
        { x, y } = board.player
        { drawStyle } = editor

        offBoard direction =
            case direction of
                Right -> x + 1 >= width
                Left  -> x <= 0
                Up    -> y + 1 >= height
                Down  -> y <= 0

        removeWall direction = updateTileBoundary ( x, y ) direction Alley
        restoreWall direction = updateTileBoundary ( x, y ) direction Wall
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
                                |> updatePlayer ( movePlayer direction )
                                |> restoreWall Up
                                |> restoreWall Down
                                |> restoreWall Left
                                |> restoreWall Right

                        else board
                                |> updatePlayer ( movePlayer direction )
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
        startTileToPlayer (x, y, tile) = if tile.tileType == Start
            then Just (Player x y Up)
            else Nothing

        player = tilesWithIndex board
            |> List.filterMap startTileToPlayer
            |> List.head
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

updatePlayer : ( Player -> Player ) -> Board -> Board
updatePlayer update board =
    { board | player = update board.player}

queryTile : ( Int, Int ) -> Board -> (Tile -> Bool) -> Bool
queryTile ( x, y ) { width, height, tiles } query =
    Array.get ((height - 1 - y) * width + x) tiles
        |> Maybe.map query
        |> Maybe.withDefault True

updateTile : ( Int, Int ) -> (Tile -> Tile) -> Board -> Board
updateTile ( x, y ) update board =
    let
        { width, height, tiles } = board
        i = (height - 1 - y) * width + x
        set tile = { board | tiles = Array.set i tile tiles }
    in
        Array.get i tiles
            |> Maybe.map (update >> set)
            |> Maybe.withDefault board

updateTileType : TileType -> Tile -> Tile
updateTileType tileType tile = { tile | tileType = tileType }

updateTileBoundary : ( Int, Int ) -> Direction -> Boundary -> Board -> Board
updateTileBoundary ( x, y ) direction boundary board =
    let
        update dir tile =
            case dir of
                Up    -> { tile | top = boundary }
                Down  -> { tile | bottom = boundary }
                Left  -> { tile | left = boundary }
                Right -> { tile | right = boundary }

        neighbour =
            case direction of
                Up    -> ( x, y + 1 )
                Down  -> ( x, y - 1 )
                Left  -> ( x - 1, y )
                Right -> ( x + 1, y )
    in
    board
        |> updateTile ( x, y ) (update direction)
        |> updateTile neighbour (update <| oppositeDirection direction)

hasBoundary : Direction -> Boundary -> Tile -> Bool
hasBoundary direction boundary tile =
    boundary == case direction of
        Right -> tile.right
        Left  -> tile.left
        Up    -> tile.top
        Down  -> tile.bottom

viewGame : Game solver -> List (Html Msg)
viewGame game =
    let
        { board, programmer, animation } = game
        { player } = board

        angle = case player.orientation of
            Left  -> -pi / 2
            Up    -> pi
            Right -> pi / 2
            Down  -> 0

        renderPlayer =
            Text.fromString "T"
                |> Text.color Color.blue
                |> Text.weight Text.Bold
                |> Text.size Text.huge
                |> rendered
                |> rotate ( angle + animation.playerOrientation animation.t )
                |> shiftX ( 50 * ( toFloat player.x + animation.playerX animation.t ) )
                |> shiftY ( 50 * ( toFloat player.y + animation.playerY animation.t ) )

        renderTile ( x, y, tile ) = viewTile ( x, y ) tile
    in
        [ renderPlayer ::
            ( tilesWithIndex board
                |> List.map renderTile
            )
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

tilesWithIndex : Board -> List ( Int, Int, Tile )
tilesWithIndex { width, height, tiles } =
    tiles
        |> Array.indexedMap
            (\i tile ->
                ( modBy width i, height - 1 - i // width, tile )
            )
        |> Array.toList


viewTile : ( Int, Int ) -> Tile -> Collage Msg
viewTile ( x, y ) { tileType, left, top, bottom, right } =
    let
        wallStyle wall =
            case wall of
                Wall  -> solid thin (uniform Color.black)
                Alley -> invisible

        tile =
            case tileType of
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
                tile
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
            \game -> Sub.batch
                [ onKeyDown keyDownDecoder
                , onKeyUp keyUpDecoder
                , case game.mode of
                    Execute
                        -> Time.every 400 Tick
                    Program
                        -> if game.animation.t < 1
                                then onAnimationFrameDelta AnimationFrame
                                else Sub.none
                    _
                        -> Sub.none
                ]
        , init = \_ -> initGame configuration
        , update = updateGame
        , view =
            \game ->
                { title = "ElMaze"
                , body = viewGame game
                }
        }

