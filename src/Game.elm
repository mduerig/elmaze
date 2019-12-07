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

type alias Game s =
    { board : Board s
    , mode : Mode
    , recordingEnabled : Bool
    , programText : String
    }

type alias Configuration s =
  { board : Board s
  , init : Board s -> String -> s
  , update : Board s -> s -> ( s, Move )
  }

type Move
    = Forward
    | TurnLeft
    | TurnRight
    | Nop

type alias Board s =
    { size : Float
    , width : Int
    , height : Int
    , tiles : Array Tile
    , actors : List ( Actor s )
    , animation : Animation
    }

type alias Animation =
    { v : Float
    , t : Float
    , onDelta : (Float -> Msg) -> Sub Msg
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

type Actor s
    = Hero HeroData
    | InputController Move
    | Executor ( ExecutionData s )

type alias ExecutionData s =
    { solver : Maybe s
    , init : Board s -> String -> s
    , update : Board s -> s -> ( s, Move )
    , move : Move
    }

type ActorReaction solver
    = None
    | Record

type HeroState
    = Moving
    | Idle
    | Won
    | Lost

type alias HeroData =
    { x : Int
    , y : Int
    , phi : Direction
    , animation : HeroAnimation
    , state : HeroState
    , moves : List Move
    }

type alias HeroAnimation =
    { dX : Float -> Float
    , dY : Float -> Float
    , dPhi : Float -> Float
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
                |> addActor
                    ( Hero
                        { x = 0
                        , y = 0
                        , phi = Up
                        , animation = noHeroAnimation
                        , state = Idle
                        , moves = []
                        }
                    )
                |> addActor
                    ( Executor
                        { solver = Nothing
                        , init = init
                        , update = update
                        , move = Nop
                        }
                    )
                |> addActor ( InputController Nop )
            , mode = Program
            , recordingEnabled = True
            , programText = ""
            }
    in
        ( game, getProgramTextareaWidth )

getProgramTextareaWidth : Cmd Msg
getProgramTextareaWidth =
    Task.attempt GotViewport ( Dom.getViewportOf "boardWidth" )

noHeroAnimation : HeroAnimation
noHeroAnimation =
    { dY = always 0
    , dX = always 0
    , dPhi = always 0
    }

newBoard : Int -> Int -> Board s
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
    , actors = []
    , animation =
        { v = 1.5
        , t = 0
        , onDelta = always Sub.none
        }
    }

updateGame : Msg -> Game solver -> ( Game solver, Cmd Msg )
updateGame msg  ( { board, programText } as game ) =
    let
        initHero mode = if mode == Execute
            then resetHero
            else identity

        initSolver executor board0 = Just
            <| executor.init board0
            <| ensureTrailingLF
            <| programText

        initExecutor mode board0 =
            board0 |> mapActors
                (\actor ->
                    case actor of
                        Executor executor ->
                            if mode == Execute then
                                Executor { executor | solver = initSolver executor board0 }
                            else
                                    Executor { executor | solver = Nothing }
                        _ -> actor
                )
    in
        case msg of
            ResetGame ->
                ( { game
                  | board = board |> resetHero
                  , programText = ""
                  , mode = Program
                }
                , Cmd.none
                )

            EnableRecording recordingOn ->
                ( { game | recordingEnabled = recordingOn }
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
                    t = board.animation.t
                in
                    if t == 0 then
                        updateGame AnimationStart { game | board = board |> advanceAnimation dt }
                    else if t < 1 then
                        updateGame AnimationStep  { game | board = board |> advanceAnimation dt }
                    else
                        updateGame AnimationEnd   { game | board = board |> stopAnimation }

            ProgramChanged newProgram ->
                ( { game | programText = newProgram }
                , Cmd.none
                )

            SwitchMode mode ->
                updateGame EnterMode
                    { game
                    | mode = mode
                    , board = board
                        |> initHero mode
                        |> initExecutor mode
                    }

            _ ->
                ( game.board.actors
                    |> List.foldl ( updateActor msg ) game
                    |> applyStateAndAnimation
                , Cmd.none
                )

applyStateAndAnimation : Game s -> Game s
applyStateAndAnimation ( { board } as game ) =
    let
        state = getHero board.actors
            |> Maybe.map .state

        programText = getProgramText board.actors
    in
        { game
        | mode = if state == Just Moving
            then Execute
            else Program
        , board = board
            |> if state == Just Idle && game.mode == Execute
                then identity
                else startAnimation False
        , programText = programText
        }

updateActor : Msg -> Actor s -> Game s -> Game s
updateActor msg actor game =
    let
        ( updatedActor, reaction ) =
            case actor of
                Hero heroData ->
                    updateHeroData msg game heroData

                InputController _ ->
                    updateInputController msg

                Executor executor ->
                    updateExecutor msg game.board executor
    in
        { game | board = setActor updatedActor game.board }
            |> applyReaction reaction

addActor : Actor s -> Board s -> Board s
addActor actor ( {actors } as board ) =
    { board | actors = actor :: actors }

mapActors : ( Actor s -> Actor s ) -> Board s -> Board s
mapActors update board =
    { board
    | actors = board.actors
        |> List.map update
    }

setActor : Actor s -> Board s -> Board s
setActor actor board =
    board |> mapActors
        ( \currentActor ->
            case ( currentActor, actor ) of
                ( Hero _, Hero _ ) -> actor
                ( InputController _, InputController _ ) -> actor
                ( Executor _, Executor _ ) -> actor
                ( _, _ ) -> currentActor
        )

getProgramText : List ( Actor s ) -> String
getProgramText actors =
    let
        toString direction =
            case direction of
                Forward   -> "forward"
                TurnLeft  -> "left"
                TurnRight -> "right"
                _         -> ""
    in
        getHero actors
            |> Maybe.map .moves
            |> Maybe.map List.reverse
            |> Maybe.map ( List.map toString )
            |> Maybe.map (String.join "\n")
            |> Maybe.map ensureTrailingLF
            |> Maybe.withDefault ""

applyReaction : ActorReaction s -> Game s -> Game s
applyReaction reaction ( { board } as game ) =
    game

updateInputController : Msg -> ( Actor s, ActorReaction s)
updateInputController msg =
    case msg of
       KeyArrow Up    -> ( InputController Forward, None )
       KeyArrow Left  -> ( InputController TurnLeft, None )
       KeyArrow Right -> ( InputController TurnRight, None )
       _              -> ( InputController Nop, None )

updateHeroData : Msg -> Game s -> HeroData -> ( Actor s, ActorReaction s )
updateHeroData msg game hero =
    case game.mode of
        Program ->
            updateHeroDataProgramMode msg game hero

        Execute ->
            updateHeroDataExecuteMode msg game hero

updateHeroDataProgramMode : Msg -> Game s ->  HeroData -> ( Actor s, ActorReaction s )
updateHeroDataProgramMode msg { board, recordingEnabled } hero =
    let
        { x, y, phi } = hero

        isBlocked direction =
            queryTile ( x, y ) board ( hasBoundary direction Wall )
    in
        if not recordingEnabled
            then
                ( Hero hero, None )
            else if msg == AnimationEnd then
                ( Hero
                    <| animateHero noHeroAnimation hero
                , Record
                )
            else case getInput board.actors of
                Just Forward ->
                    if isBlocked phi then
                        ( Hero hero, None )
                    else
                        ( Hero
                            <| moveHero phi
                                << animateHero ( moveHeroAnimation phi )
                            <| recordMove Up
                            <| hero
                        , Record
                        )

                Just TurnLeft ->
                    ( Hero
                        <| turnHero Left
                            << animateHero ( turnHeroAnimation Left )
                        <| recordMove Left
                        <| hero
                    , Record
                    )

                Just TurnRight ->
                    ( Hero
                        <| turnHero Right
                            << animateHero ( turnHeroAnimation Right )
                        <| recordMove Right
                        <| hero
                    , Record
                    )

                _ -> ( Hero hero, None )

recordMove : Direction -> HeroData -> HeroData
recordMove direction hero =
    { hero | moves = directionToMove direction :: hero.moves }

updateExecutor : Msg -> Board s -> ExecutionData s -> ( Actor s, ActorReaction s )
updateExecutor msg board executor =
    let
        ( updatedSolver, move ) = case executor.solver of
            Just solver ->
                executor.update board solver
                    |> Tuple.mapFirst Just

            _ ->
                ( Nothing, Nop )
    in
        if msg == EnterMode || msg == AnimationEnd then
            ( Executor
                { executor
                | solver = updatedSolver
                , move = move
                }
            , None
            )
        else
            ( Executor { executor | move = Nop }
            , None )

updateHeroDataExecuteMode : Msg -> Game s -> HeroData -> ( Actor s, ActorReaction s )
updateHeroDataExecuteMode msg { board } hero =
    let
        { x, y, phi } = hero

        isFree direction =
            queryTile ( x, y ) board ( hasBoundary direction Path )

        atGoal = isHeroAtGoal hero board
        winAnimation =
            { noHeroAnimation
            | dY = Ease.outBack >> \t -> t
            , dPhi = \t -> 4 * pi * t
            }

        loseAnimation =
            { noHeroAnimation
            | dY = Ease.inBack >> \t -> -10 * t
            , dPhi = \t -> 4 * pi * t
            }
    in
        case getMove board.actors of
            Just Forward ->
                if isFree phi
                    then
                        ( hero
                            |> moveHero phi
                            >> animateHero ( moveHeroAnimation phi )
                            |> setHeroState Moving
                            |> Hero
                        , None
                        )
                    else
                        ( hero
                            |> animateHero loseAnimation
                            |> setHeroState Lost
                            |> Hero
                        , None
                        )

            Just TurnLeft ->
                ( hero
                    |> turnHero Left
                    >> animateHero (turnHeroAnimation Left)
                    |> setHeroState Moving
                    |> Hero
                , None
                )

            Just TurnRight ->
                ( hero
                    |> turnHero Right
                        >> animateHero (turnHeroAnimation Right)
                    |> setHeroState Moving
                    |> Hero
                , None
                )

            _ ->
                if msg == AnimationEnd then
                    if atGoal then
                        ( hero
                            |> animateHero winAnimation
                            |> setHeroState Won
                            |> Hero
                        , None
                        )
                    else
                        ( hero
                            |> animateHero noHeroAnimation
                            |> setHeroState Idle
                            |> Hero
                        , None
                        )
                else
                    ( Hero hero, None )

setHeroState : HeroState -> HeroData -> HeroData
setHeroState state hero =
    { hero | state = state }

stopAnimation : Board s -> Board s
stopAnimation board =
    let animation = board.animation
    in  { board | animation = { animation | t = 0, onDelta = always Sub.none }}

startAnimation : Bool -> Board s -> Board s
startAnimation restart board =
    let
        animation = board.animation
    in
        if restart || animation.onDelta AnimationFrame == Sub.none then
            { board | animation = { animation | t = 0, onDelta = onAnimationFrameDelta }}
        else
            board

advanceAnimation : Float -> Board s -> Board s
advanceAnimation dt board =
    let animation = board.animation
    in  { board | animation = { animation | t = animation.t + animation.v * dt / 1000 }}

moveHeroAnimation : Direction -> HeroAnimation
moveHeroAnimation direction =
    case direction of
        Left  -> { noHeroAnimation | dX = Ease.inOutBack >> \t -> 1 - t }
        Right -> { noHeroAnimation | dX = Ease.inOutBack >> \t -> t - 1 }
        Up    -> { noHeroAnimation | dY = Ease.inOutBack >> \t -> t - 1 }
        Down  -> { noHeroAnimation | dY = Ease.inOutBack >> \t -> 1 - t }

turnHeroAnimation : Direction -> HeroAnimation
turnHeroAnimation direction =
    case direction of
        Left  -> { noHeroAnimation | dPhi = Ease.inOutBack >> \t -> pi/2 * (t - 1) }
        Right -> { noHeroAnimation | dPhi = Ease.inOutBack >> \t -> pi/2 * (1 - t) }
        _     ->   noHeroAnimation

isHeroAtGoal : HeroData -> Board s -> Bool
isHeroAtGoal hero board =
    queryTile (hero.x, hero.y) board (\tile -> tile.tileType == Goal)

resetHero : Board s -> Board s
resetHero ( { actors } as board ) =
    let
        startTileToHero (x, y, tile) = if tile.tileType == Start
            then Just ( HeroData x y Up noHeroAnimation Idle [] )
            else Nothing

        hero = tilesWithIndex board
            |> List.filterMap startTileToHero
            |> List.head
            |> Maybe.withDefault ( HeroData 0 0 Up noHeroAnimation Idle [] )
    in
        { board | actors = setHero hero actors }

setHero : HeroData -> List ( Actor s ) -> List ( Actor s )
setHero heroData actors =
    let
        replaceHero actor =
            case actor of
                Hero _ -> Hero heroData
                _ -> actor
    in
        actors
            |> List.map replaceHero

moveHero : Direction -> HeroData -> HeroData
moveHero direction hero =
    case direction of
        Right -> { hero | x = hero.x + 1 }
        Left  -> { hero | x = hero.x - 1 }
        Up    -> { hero | y = hero.y + 1 }
        Down  -> { hero | y = hero.y - 1 }

turnHero : Direction -> HeroData -> HeroData
turnHero direction hero =
    { hero | phi =
        case direction of
            Right -> rightOfDirection hero.phi
            Left  -> leftOfDirection hero.phi
            _     -> hero.phi
    }

animateHero : HeroAnimation -> HeroData -> HeroData
animateHero animation hero =
    { hero | animation = animation }

queryTile : ( Int, Int ) -> Board s -> (Tile -> Bool) -> Bool
queryTile ( x, y ) { width, height, tiles } query =
    Array.get ((height - 1 - y) * width + x) tiles
        |> Maybe.map query
        |> Maybe.withDefault True

updateTile : ( Int, Int ) -> (Tile -> Tile) -> Board s -> Board s
updateTile ( x, y ) update ( { width, height, tiles } as board ) =
    let
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

updateTileBoundary : ( Int, Int ) -> Direction -> Boundary -> Board s -> Board s
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

getActor : ( Actor s -> Maybe a ) -> List ( Actor s ) -> Maybe a
getActor get actors =
    actors
        |> List.filterMap get
        |> List.head

getMove : List ( Actor s ) -> Maybe Move
getMove actors =
    let
        executor actor =
            case actor of
                Executor ex -> Just ex.move
                _ -> Nothing
    in
        actors |> getActor executor

getInput : List ( Actor s ) -> Maybe Move
getInput actors =
    let
        inputController actor =
            case actor of
                InputController move -> Just move
                _ -> Nothing

    in
        actors |> getActor inputController

getHero : List ( Actor s ) -> Maybe HeroData
getHero actors =
    let
        hero actor =
            case actor of
                Hero heroData -> Just heroData
                _ -> Nothing

    in
        actors |> getActor hero

viewActor : Float -> Float -> Actor s -> Collage Msg
viewActor t cellSize actor =
    case actor of
        Hero hero -> viewHero t cellSize hero
        InputController _ -> Collage.group []
        Executor _ -> Collage.group []


viewHero : Float -> Float -> HeroData -> Collage Msg
viewHero t cellSize hero =
    let
        angle = case hero.phi of
            Left  -> pi/2
            Up    -> 0
            Right -> -pi/2
            Down  -> pi

        dPhi = hero.animation.dPhi t
        dX = hero.animation.dX t
        dY = hero.animation.dY t
    in
        [ Text.fromString "🐞"
            |> Text.size (round (cellSize/5*3))
            |> rendered
            |> rotate ( angle + dPhi)
        , circle (cellSize/5*2)
            |> filled transparent
        ]
        |> group
        |> shiftX ( cellSize * ( toFloat hero.x + dX ) )
        |> shiftY ( cellSize * ( toFloat hero.y + dY ) )

viewGame : Game solver -> List (Html Msg)
viewGame { board, programText, mode } =
    let
        { actors, animation } = board
        cellSize = board.size / toFloat board.width

        viewActors = actors
            |> List.map ( viewActor animation.t cellSize )
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
                    [ viewActors ++
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
                            , Textarea.value <| programText
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

directionToMove : Direction -> Move
directionToMove direction =
    case direction of
        Up    -> Forward
        Left  -> TurnLeft
        Right -> TurnRight
        _     -> Nop

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

tilesWithIndex : Board s -> List ( Int, Int, Tile )
tilesWithIndex { width, height, tiles } =
    tiles
        |> Array.indexedMap
            (\i tile ->
                ( modBy width i, height - 1 - i // width, tile )
            )
        |> Array.toList

viewTile : Float -> ( Int, Int, Tile ) -> Collage Msg
viewTile size ( x, y, { tileType, background } )=
    let
        tile =
            case tileType of
                Goal ->
                    [ Text.fromString "🌺"
                        |> Text.size (round (size/5*3))
                        |> rendered
                    ]

                Start ->
                    [ Text.fromString "🌟"
                        |> Text.size (round (size/5*4))
                        |> rendered
                        |> shiftY (-size/5)
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
            \{ board } -> Sub.batch
                [ onKeyDown keyDownDecoder
                , onKeyUp keyUpDecoder
                , board.animation.onDelta AnimationFrame
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

