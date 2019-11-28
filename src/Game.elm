module Game exposing (..)

import Array exposing ( Array )
import Browser
import Browser.Dom as Dom
import Browser.Events exposing ( onKeyDown, onKeyUp, onAnimationFrameDelta, onResize )
import Collage exposing ( .. )
import Collage.Render exposing ( svg )
import Collage.Text as Text
import Color
import Html exposing ( Html )
import Html.Attributes as Attr
import Html.Events as Events
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Json.Decode as Decode
import Task as Task
import Ease

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
    , onDelta : (Float -> Msg) -> Sub Msg
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
    { size : Float
    , width : Int
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
    { recordingEnabled : Bool
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
    , mode : Mode
    , animation : Animation
    , executor : Executor solver
    }

type Mode
    = Edit
    | Record
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
    | ResetGame
    | SwitchMode Mode
    | EnterMode
    | KeyOtherDown String
    | KeyOtherUp String
    | Resize Int Int
    | AnimationFrame Float
    | AnimationStart
    | AnimationStep
    | AnimationEnd
    | ProgramChanged String
    | GotViewport ( Result Dom.Error Dom.Viewport )
    | EnableRecording Bool

initGame : Configuration solver -> ( Game solver, Cmd Msg )
initGame { board, init, update } =
    let
        game =
            { board = board
                |> playerAtStart
            , mode = Record
            , animation = noAnimation
            , editor =
                { drawStyle = Alley
                }
            , programmer =
                { recordingEnabled = True
                , program = ""
                }
            , executor =
                { solver = Nothing
                , init = init
                , update = update
                }
            }
    in
        ( game, getProgramTextareaWidth )

getProgramTextareaWidth : Cmd Msg
getProgramTextareaWidth =
    Task.attempt GotViewport ( Dom.getViewportOf "boardWidth" )

startAnimation : Animation
startAnimation =
    { v = 1.5
    , t = 0
    , playerX = always 0
    , playerY = always 0
    , playerOrientation = always 0
    , onDelta = onAnimationFrameDelta
    }

noAnimation : Animation
noAnimation =
    { startAnimation
    | onDelta = always Sub.none
    }

newBoard : Int -> Int -> Board
newBoard width height =
    { size = 500
    , width = width
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

        updatePlayerPos mode = if mode == Execute
            then playerAtStart board
            else board

        updateExecutor mode = if mode == Execute
            then { executor
                 | solver = Just
                     <| executor.init board
                     <| ensureTrailingLF
                     <| programmer.program
                 }
            else { executor | solver = Nothing }
    in
        case msg of
            ResetGame ->
                ( { game
                  | board = board |> playerAtStart
                  , programmer = { programmer | program = "" }
                }
                , Cmd.none
                )


            EnableRecording recordingOn ->
                ( { game | programmer = { programmer | recordingEnabled = recordingOn } }
                , Cmd.none
                )

            GotViewport ( Ok { viewport } ) ->
                ( { game | board = { board | size = viewport.width } }
                , Cmd.none
                )

            Resize _ _  ->
                ( game, getProgramTextareaWidth )

            AnimationFrame dt ->
                let
                    nextStep =
                        { game
                        | animation = { animation | t = animation.t + animation.v * dt / 1000 }
                        }
                in
                    if animation.t == 0 then
                        updateGame AnimationStart nextStep
                    else if nextStep.animation.t < 1 then
                        updateGame AnimationStep nextStep
                    else
                        updateGame AnimationEnd { game | animation = noAnimation }

            ProgramChanged newProgram ->
                ( { game | programmer = { programmer | program = newProgram } }
                , Cmd.none
                )

            SwitchMode mode ->
                updateGame EnterMode
                    { game
                    | mode = mode
                    , board = updatePlayerPos mode
                    , executor = updateExecutor mode
                    }

            _ ->
                case game.mode of
                    Record -> ( updateGameProgramMode msg game, Cmd.none )
                    Edit    -> ( updateGameEditMode msg game, Cmd.none )
                    Execute -> ( updateGameExecuteMode msg game, Cmd.none)

updateGameExecuteMode : Msg -> Executing a s -> Executing a s
updateGameExecuteMode msg game =
    let
        { board, executor } = game
        { x, y, orientation } = board.player

        ( updatedSolver, move ) = case executor.solver of
            Just solver ->
                executor.update board solver
                    |> Tuple.mapFirst Just

            _ ->
                ( Nothing, Nop )

        isFree direction =
            queryTile ( x, y ) board ( hasBoundary direction Alley )

        ( movedPlayer, animation ) = case move of
            Forward ->
                if isFree orientation
                    then
                        ( updatePlayer ( movePlayer orientation )
                        , movePlayerAnimation orientation
                        )
                    else
                        ( identity, noAnimation )

            TurnLeft ->
                ( updatePlayer (turnPlayer Left)
                , turnPlayerAnimation Left
                )

            TurnRight ->
                ( updatePlayer (turnPlayer Right)
                , turnPlayerAnimation Right
                )

            _ ->
                ( identity, noAnimation )
    in
        if msg == EnterMode || msg == AnimationEnd
        then
            { game
            | board = movedPlayer board
            , mode = if move == Nop
                then Record
                else Execute
            , animation = animation
            , executor = { executor | solver = updatedSolver }
            }
        else
            game

updateGameProgramMode : Msg -> Programming a -> Programming a
updateGameProgramMode msg game =
    let
        { board, programmer } = game
        { x, y, orientation } = board.player
        { program, recordingEnabled } = programmer

        isBlocked direction =
            queryTile ( x, y ) board ( hasBoundary direction Wall )
    in
        if not recordingEnabled
            then game
            else case msg of
                KeyArrow Down ->
                    game

                KeyArrow Up ->
                    if isBlocked orientation then
                        game
                    else
                        { game
                        | board = updatePlayer ( movePlayer orientation ) board
                        , animation = movePlayerAnimation orientation
                        , programmer = { programmer | program = appendMove program Up }
                        }

                KeyArrow direction ->
                    { game
                    | board = updatePlayer ( turnPlayer direction ) board
                    , animation = turnPlayerAnimation direction
                    , programmer = { programmer | program = appendMove program direction }
                    }

                _ -> game

movePlayerAnimation : Direction -> Animation
movePlayerAnimation direction =
    case direction of
        Left  -> { startAnimation | playerX = Ease.inOutBack >> \t -> 1 - t }
        Right -> { startAnimation | playerX = Ease.inOutBack >> \t -> t - 1 }
        Up    -> { startAnimation | playerY = Ease.inOutBack >> \t -> t - 1 }
        Down  -> { startAnimation | playerY = Ease.inOutBack >> \t -> 1 - t }

turnPlayerAnimation : Direction -> Animation
turnPlayerAnimation direction =
    case direction of
        Left  -> { startAnimation | playerOrientation = Ease.inOutBack >> \t -> pi/2 * (t - 1) }
        Right -> { startAnimation | playerOrientation = Ease.inOutBack >> \t -> pi/2 * (1 - t) }
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
            KeyShift True ->
                { game | editor = { editor | drawStyle = Wall }}

            KeyShift False ->
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
        { board, programmer, animation, mode } = game
        { player } = board
        cellSize = board.size / toFloat board.width

        angle = case player.orientation of
            Left  -> pi/2
            Up    -> 0
            Right -> -pi/2
            Down  -> pi

        viewPlayer =
            [ Text.fromString "🐞"
                |> Text.size (round (cellSize/5*3))
                |> rendered
                |> rotate ( angle + animation.playerOrientation animation.t )
            , circle (cellSize/5*2)
                |> filled transparent
            ]
            |> group
            |> shiftX ( cellSize * ( toFloat player.x + animation.playerX animation.t ) )
            |> shiftY ( cellSize * ( toFloat player.y + animation.playerY animation.t ) )
    in
        [ CDN.stylesheet
        , Grid.containerFluid []
            [ Grid.row []
                [ Grid.col [] []
                , Grid.col []
                    [ viewPlayer ::
                        ( tilesWithIndex board
                            |> List.map  ( viewTile cellSize )
                        )
                        |> group
                        |> svg
                    ]
                , Grid.col []
                    [ Html.div []
                        [ Textarea.textarea
                            [ Textarea.rows 15
                            , Textarea.value <| programmer.program
                            , Textarea.onInput ProgramChanged
                            , Textarea.attrs
                                [ Events.onFocus <| EnableRecording False
                                , Events.onBlur <| EnableRecording True
                                ]
                            ]
                        , if mode == Execute
                            then Button.button
                                [ Button.danger
                                , Button.block
                                , Button.onClick <| SwitchMode Record
                                ]
                                [ Html.text "stop" ]
                            else Button.button
                                [ Button.outlineSuccess
                                , Button.block
                                , Button.disabled <| mode /= Record
                                , Button.onClick <| SwitchMode Execute ]
                                [ Html.text "run" ]
                        , Button.button
                            [ Button.outlineWarning
                            , Button.block
                            , Button.onClick ResetGame
                            ]
                            [ Html.text "reset game"]
                        , if mode == Edit
                            then Button.button
                                [ Button.secondary
                                , Button.block
                                , Button.onClick
                                    <| SwitchMode Record ]
                                [ Html.text "edit" ]
                            else Button.button
                                [ Button.outlineSecondary
                                , Button.block
                                , Button.onClick
                                    <| SwitchMode Edit ]
                                [ Html.text "edit" ]
                        ]
                    ]
                , Grid.col [] []
                ]
            , Grid.row []
                [ Grid.col [ ] []
                , Grid.col [ Col.xs4 ]
                    [ Html.div [ Attr.id "boardWidth" ] [] ]
                , Grid.col [ ] []
                , Grid.col [] []
                ]
            ]
        ]

appendMove : String -> Direction -> String
appendMove program direction =
    let
        command = case direction of
            Up    -> "forward\n"
            Left  -> "left\n"
            Right -> "right\n"
            _     -> ""
    in
        ensureTrailingLF program ++ command

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

ensureTrailingLF : String -> String
ensureTrailingLF s =
    if String.endsWith "\n" s || s == ""
        then s
        else s ++ "\n"

tilesWithIndex : Board -> List ( Int, Int, Tile )
tilesWithIndex { width, height, tiles } =
    tiles
        |> Array.indexedMap
            (\i tile ->
                ( modBy width i, height - 1 - i // width, tile )
            )
        |> Array.toList

viewTile : Float -> ( Int, Int, Tile ) -> Collage Msg
viewTile size ( x, y, { tileType, left, top, bottom, right } )=
    let
        wallStyle wall =
            case wall of
                Wall  -> solid thin (uniform Color.black)
                Alley -> invisible

        tile =
            case tileType of
                Goal ->
                    [ Text.fromString "🌺"
                        |> Text.size (round (size/5*3))
                        |> rendered
                    ]

                _ ->
                    []
    in
        group
            [ line size
                |> traced (wallStyle bottom)
                |> shiftY -(size/2)
            , line size
                |> traced (wallStyle top)
                |> shiftY (size/2)
            , line size
                |> traced (wallStyle right)
                |> rotate (pi/2)
                |> shiftX (size/2)
            , line size
                |> traced (wallStyle left)
                |> rotate (pi/2)
                |> shiftX -(size/2)
            , group
                tile
            , square size
                |> filled (uniform Color.lightYellow)
            ]
            |> shift ( size * toFloat x, size * toFloat y )

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
            \{ animation } -> Sub.batch
                [ onKeyDown keyDownDecoder
                , onKeyUp keyUpDecoder
                , animation.onDelta AnimationFrame
                , onResize Resize
                ]
        , init = \_ -> initGame configuration
        , update = updateGame
        , view =
            \game ->
                { title = "Elmaze"
                , body = viewGame game
                }
        }

