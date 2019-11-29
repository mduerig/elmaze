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
    , program :
        { recordingEnabled : Bool
        , text : String
        }
    , executor :
        { solver : Maybe solver
        , init : Board -> String -> solver
        , update : Board -> solver -> ( solver, Move )
        }
    }

type alias Animation =
    { v : Float
    , t : Float
    , heroX : Float -> Float
    , heroY : Float -> Float
    , heroOrientation : Float -> Float
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
    , hero : Hero
    , tiles : Array Tile
    }

type Mode
    = Program
    | Execute

type alias Tile =
    { tileType : TileType
    , background : Maybe String
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
    | Path

type alias Hero =
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
    | KeyEscape
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
                |> heroAtStart
            , mode = Program
            , animation = noAnimation
            , program =
                { recordingEnabled = True
                , text = ""
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
    , heroY = always 0
    , heroX = always 0
    , heroOrientation = always 0
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
            , background = Nothing
            , top = Wall
            , left = Wall
            , bottom = Wall
            , right = Wall
            }
    , hero = Hero 0 0 Up
    }

updateGame : Msg -> Game solver -> ( Game solver, Cmd Msg )
updateGame msg game =
    let
        { board, program, executor, animation } = game

        updateHeroPos mode = if mode == Execute
            then heroAtStart board
            else board

        updateExecutor mode = if mode == Execute
            then { executor
                 | solver = Just
                     <| executor.init board
                     <| ensureTrailingLF
                     <| program.text
                 }
            else { executor | solver = Nothing }
    in
        case msg of
            ResetGame ->
                ( { game
                  | board = board |> heroAtStart
                  , program = { program | text = "" }
                  , mode = Program
                }
                , Cmd.none
                )


            EnableRecording recordingOn ->
                ( { game | program = { program | recordingEnabled = recordingOn } }
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
                ( { game | program = { program | text = newProgram } }
                , Cmd.none
                )

            SwitchMode mode ->
                updateGame EnterMode
                    { game
                    | mode = mode
                    , board = updateHeroPos mode
                    , executor = updateExecutor mode
                    }

            _ ->
                case game.mode of
                    Program -> ( updateGameProgramMode msg game, Cmd.none )
                    Execute -> ( updateGameExecuteMode msg game, Cmd.none)

updateGameExecuteMode : Msg -> Game s -> Game s
updateGameExecuteMode msg game =
    let
        { board, executor } = game
        { hero } = board
        { x, y, orientation } = hero

        ( updatedSolver, move ) = case executor.solver of
            Just solver ->
                executor.update board solver
                    |> Tuple.mapFirst Just

            _ ->
                ( Nothing, Nop )

        isFree direction =
            queryTile ( x, y ) board ( hasBoundary direction Path )

        atGoal = isHeroAtGoal hero board
        winAnimation = { startAnimation
                        | v = 1
                        , heroY = Ease.outBack >> \t -> t
                        , heroOrientation = \t -> 4 * pi * t
                        }

        loseAnimation = { startAnimation
                        | v = 1
                        , heroY = Ease.inBack >> \t -> -10 * t
                        , heroOrientation = \t -> 4 * pi * t
                        }

        ( movedHero, animation, done ) = case move of
            Forward ->
                if isFree orientation
                    then
                        ( updateHero ( moveHero orientation )
                        , moveHeroAnimation orientation
                        , False
                        )
                    else
                        ( identity, loseAnimation, True )

            TurnLeft ->
                ( updateHero (turnHero Left)
                , turnHeroAnimation Left
                , False
                )

            TurnRight ->
                ( updateHero (turnHero Right)
                , turnHeroAnimation Right
                , False
                )

            Nop ->
                ( identity
                , if atGoal
                    then winAnimation
                    else noAnimation
                , True
                )
    in
        if msg == EnterMode || msg == AnimationEnd
        then
            { game
            | board = movedHero board
            , mode = if done
                then Program
                else Execute
            , animation = animation
            , executor = { executor | solver = updatedSolver }
            }
        else
            game

updateGameProgramMode : Msg -> Game a -> Game a
updateGameProgramMode msg game =
    let
        { board, program } = game
        { x, y, orientation } = board.hero

        isBlocked direction =
            queryTile ( x, y ) board ( hasBoundary direction Wall )
    in
        if not program.recordingEnabled
            then game
            else case msg of
                KeyArrow Down ->
                    game

                KeyArrow Up ->
                    if isBlocked orientation then
                        game
                    else
                        { game
                        | board = updateHero ( moveHero orientation ) board
                        , animation = moveHeroAnimation orientation
                        , program = { program | text = appendMove program.text Up }
                        }

                KeyArrow direction ->
                    { game
                    | board = updateHero ( turnHero direction ) board
                    , animation = turnHeroAnimation direction
                    , program = { program | text = appendMove program.text direction }
                    }

                _ -> game

moveHeroAnimation : Direction -> Animation
moveHeroAnimation direction =
    case direction of
        Left  -> { startAnimation | heroX = Ease.inOutBack >> \t -> 1 - t }
        Right -> { startAnimation | heroX = Ease.inOutBack >> \t -> t - 1 }
        Up    -> { startAnimation | heroY = Ease.inOutBack >> \t -> t - 1 }
        Down  -> { startAnimation | heroY = Ease.inOutBack >> \t -> 1 - t }

turnHeroAnimation : Direction -> Animation
turnHeroAnimation direction =
    case direction of
        Left  -> { startAnimation | heroOrientation = Ease.inOutBack >> \t -> pi/2 * (t - 1) }
        Right -> { startAnimation | heroOrientation = Ease.inOutBack >> \t -> pi/2 * (1 - t) }
        _     ->   startAnimation

isHeroAtGoal : Hero -> Board -> Bool
isHeroAtGoal hero board =
    queryTile (hero.x, hero.y) board (\tile -> tile.tileType == Goal)

heroAtStart : Board -> Board
heroAtStart board =
    let
        startTileToHero (x, y, tile) = if tile.tileType == Start
            then Just (Hero x y Up)
            else Nothing

        hero = tilesWithIndex board
            |> List.filterMap startTileToHero
            |> List.head
            |> Maybe.withDefault (Hero 0 0 Up)
    in
        { board | hero = hero }

moveHero : Direction -> Hero -> Hero
moveHero direction hero =
    case direction of
        Right -> { hero | x = hero.x + 1 }
        Left  -> { hero | x = hero.x - 1 }
        Up    -> { hero | y = hero.y + 1 }
        Down  -> { hero | y = hero.y - 1 }

turnHero : Direction -> Hero -> Hero
turnHero direction hero =
    { hero | orientation =
        case direction of
            Right -> rightOfDirection hero.orientation
            Left  -> leftOfDirection hero.orientation
            _     -> hero.orientation
        }

updateHero : ( Hero -> Hero ) -> Board -> Board
updateHero update board =
    { board | hero = update board.hero}

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

updateTileBackground : String -> Tile -> Tile
updateTileBackground background tile = { tile | background = Just background }

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
        { board, program, animation, mode } = game
        { hero } = board
        cellSize = board.size / toFloat board.width

        angle = case hero.orientation of
            Left  -> pi/2
            Up    -> 0
            Right -> -pi/2
            Down  -> pi

        viewHero =
            [ Text.fromString "🐞"
                |> Text.size (round (cellSize/5*3))
                |> rendered
                |> rotate ( angle + animation.heroOrientation animation.t )
            , circle (cellSize/5*2)
                |> filled transparent
            ]
            |> group
            |> shiftX ( cellSize * ( toFloat hero.x + animation.heroX animation.t ) )
            |> shiftY ( cellSize * ( toFloat hero.y + animation.heroY animation.t ) )
    in
        [ CDN.stylesheet
        , Grid.containerFluid []
            [ Grid.row []
                [ Grid.col [] []
                , Grid.col [] [ Html.h1 [] [ Html.text "Elmaze"] ]
                , Grid.col [] []
                ]
            , Grid.row []
                [ Grid.col [] []
                , Grid.col []
                    [ viewHero ::
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
                            , Textarea.value <| program.text
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
                                , Button.onClick <| SwitchMode Program
                                ]
                                [ Html.text "Stop" ]
                            else Button.button
                                [ Button.outlineSuccess
                                , Button.block
                                , Button.disabled <| mode /= Program
                                , Button.onClick <| SwitchMode Execute ]
                                [ Html.text "Go!" ]
                        , Button.button
                            [ Button.outlineWarning
                            , Button.block
                            , Button.disabled <| mode == Execute
                            , Button.onClick ResetGame
                            ]
                            [ Html.text "Reset"]
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
viewTile size ( x, y, { tileType, background, left, top, bottom, right } )=
    let
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
            [ group
                tile
            , background
                |> Maybe.map
                     ( image (size, size) )
                |> Maybe.withDefault
                     ( square size |> filled (uniform Color.lightYellow) )
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
                "Escape"     -> KeyEscape
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

