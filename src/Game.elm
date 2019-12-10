module Game exposing (..)

import Array exposing ( Array )
import Browser
import Browser.Dom as Dom
import Browser.Events exposing ( onKeyDown, onAnimationFrameDelta, onResize )
import Random
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
    , coding : Bool
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
    | Friend FriendData
    | InputController Move
    | Executor ( ExecutionData s )

type alias ExecutionData s =
    { solver : Maybe s
    , init : Board s -> String -> s
    , update : Board s -> s -> ( s, Move )
    , move : Move
    }

type alias HeroData =
    { x : Int
    , y : Int
    , phi : Direction
    , animation : HeroAnimation
    , cmds : List Msg
    }

type alias FriendData =
    { x : Int
    , y : Int
    , phi : Direction
    , animation : HeroAnimation
    , cmds : List Msg
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
    | ResetGame
    | RunProgram
    | StopRunning
    | EnterMode
    | KeyOtherDown String
    | Resize Int Int
    | StartAnimation
    | RecordMove Move
    | AnimationFrame Float
    | AnimationStart
    | AnimationStep
    | AnimationEnd
    | ProgramChanged String
    | GotViewport ( Result Dom.Error Dom.Viewport )
    | Coding Bool
    | GenerateRandom ( Cmd Msg )
    | RandomDirection Direction

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
                        , cmds = []
                        }
                    )
                |> addActor
                    ( Friend
                        { x = 1
                        , y = 1
                        , phi = Right
                        , animation = noHeroAnimation
                        , cmds = []
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
            , coding = False
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
        initSolver board0 executor = Just
            <| executor.init board0
            <| ensureTrailingLF
            <| programText
    in
        case msg of
            GenerateRandom cmd ->
                ( game, cmd )

            ResetGame ->
                ( { game
                  | board = board |> resetHero
                  , programText = ""
                }
                , Cmd.none
                )

            Coding coding ->
                ( { game | coding = coding }
                , Cmd.none
                )

            GotViewport ( Ok { viewport } ) ->
                ( { game | board = { board | size = viewport.width } }
                , Cmd.none
                )

            Resize _ _  ->
                ( game, getProgramTextareaWidth )

            StartAnimation ->
                ( { game | board = startAnimation board }
                , Cmd.none
                )

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

            RecordMove move ->
                let
                    toString direction =
                        case direction of
                            Forward   -> "forward\n"
                            TurnLeft  -> "left\n"
                            TurnRight -> "right\n"
                            _         -> ""

                in
                    ( { game | programText = ensureTrailingLF programText ++ toString move }
                    , Cmd.none
                    )

            ProgramChanged newProgram ->
                ( { game | programText = newProgram }
                , Cmd.none
                )

            RunProgram ->
                updateGame EnterMode
                    { game
                    | board = board
                        |> resetHero
                        |> initExecutor initSolver
                    }

            StopRunning ->
                updateGame EnterMode
                    { game
                    | board = board
                        |> initExecutor ( always <| always Nothing )
                    }

            _ ->
                game.board.actors
                    |> List.foldl ( updateActor msg ) game
                    |> runCommands

initExecutor : (Board s -> ExecutionData s -> Maybe s) -> Board s -> Board s
initExecutor initSolver board =
    board |>
        mapActors
            ( \actor ->
                case actor of
                    Executor executor ->
                        Executor { executor | solver = initSolver board executor }

                    _
                        -> actor
            )

runCommands : Game s -> ( Game s , Cmd Msg )
runCommands game =
    game.board.actors
        |> List.concatMap getCommands
        |> List.foldl runCommand ( game |> clearCommands, Cmd.none )

getCommands : Actor s -> List Msg
getCommands actor =
    case actor of
        Hero hero         -> hero.cmds
        Friend friend     -> friend.cmds
        Executor        _ -> []
        InputController _ -> []

runCommand : Msg -> ( Game s, Cmd Msg ) -> ( Game s, Cmd Msg )
runCommand msg ( game, cmd ) =
    let
        ( newGame, newCommand ) = updateGame msg game
    in
        ( newGame, Cmd.batch [ cmd, newCommand ] )

updateActor : Msg -> Actor s -> Game s -> Game s
updateActor msg actor game =
    let
        updatedActor =
            case actor of
                Hero hero ->
                    updateHero msg game hero

                Friend friend ->
                    updateFriend msg game friend

                InputController _ ->
                    updateInputController msg game

                Executor executor ->
                    updateExecutor msg game.board executor
    in
        { game | board = setActor updatedActor game.board }

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
                ( Friend _, Friend _ ) -> actor
                ( InputController _, InputController _ ) -> actor
                ( Executor _, Executor _ ) -> actor
                ( _, _ ) -> currentActor
        )

updateInputController : Msg -> Game s -> Actor s
updateInputController msg { coding } =
    if coding then
        InputController Nop
    else
        case msg of
        KeyArrow Up    -> InputController Forward
        KeyArrow Left  -> InputController TurnLeft
        KeyArrow Right -> InputController TurnRight
        _              -> InputController Nop

updateExecutor : Msg -> Board s -> ExecutionData s -> Actor s
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
            Executor
                { executor
                | solver = updatedSolver
                , move = move
                }
        else
            Executor { executor | move = Nop }

updateHero : Msg -> Game s -> HeroData -> Actor s
updateHero msg { board } hero =
    let
        { x, y, phi } = hero

        isBlocked direction =
            queryTile ( x, y ) board ( hasBoundary direction Wall )

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

        running = isRunning board.actors
            |> Maybe.withDefault False

        recordMove direction game =
            game |> if running
                then identity
                else sendCommand ( RecordMove ( directionToMove direction ))

        move =
            if running then
                getMove board.actors
            else
                getInput board.actors
    in
        case move of
            Just Forward ->
                if isBlocked phi then
                    hero
                        |> animateHero loseAnimation
                        |> sendCommand StartAnimation
                        |> sendCommand StopRunning
                        |> Hero
                else
                    hero
                        |> moveHero phi
                        |> animateHero ( moveHeroAnimation phi )
                        |> sendCommand StartAnimation
                        |> recordMove Up
                        |> Hero

            Just TurnLeft ->
                hero
                    |> turnHero Left
                    |> animateHero ( turnHeroAnimation Left )
                    |> sendCommand StartAnimation
                    |> recordMove Left
                    |> Hero

            Just TurnRight ->
                hero
                    |> turnHero Right
                    |> animateHero ( turnHeroAnimation Right )
                    |> sendCommand StartAnimation
                    |> recordMove Right
                    |> Hero

            _ ->
                if msg == AnimationEnd then
                    if atGoal then
                        hero
                            |> animateHero winAnimation
                            |> ( if running
                                    then sendCommand StartAnimation
                                    else identity
                               )
                            |> sendCommand StopRunning
                            |> Hero
                    else
                        hero
                            |> animateHero noHeroAnimation
                            |> sendCommand StopRunning
                            |> Hero
                else
                    Hero hero

updateFriend : Msg -> Game s -> FriendData -> Actor s
updateFriend msg { board } ( { x, y, phi } as friend ) =
    let
        isBlocked direction =
            queryTile ( x, y ) board ( hasBoundary direction Wall )

        rnd = Random.uniform Up [ Down, Left, Right ]
    in
        case msg of
            RandomDirection direction ->
                Friend { friend | phi = direction }

            AnimationStart ->
                if isBlocked phi then
                    Friend { friend | cmds = [ GenerateRandom <| Random.generate RandomDirection rnd ] }
                else
                    friend
                        |> moveHero phi
                        |> animateHero ( moveHeroAnimation phi )
                        |> Friend

            AnimationEnd ->
                friend
                    |> animateHero noHeroAnimation
                    |> Friend

            _ ->
                Friend friend

clearCommands : Game s -> Game s
clearCommands ( { board } as game ) =
    let
        clearCmd actor = case actor of
            Hero hero         -> Hero { hero | cmds = [] }
            Friend friend     -> Friend { friend | cmds = [] }
            Executor _        -> actor
            InputController _ -> actor

    in
        { game | board = board |> mapActors clearCmd }


sendCommand : Msg -> HeroData -> HeroData
sendCommand cmd hero =
    { hero | cmds =
        if List.member cmd hero.cmds
            then hero.cmds
            else cmd :: hero.cmds }

stopAnimation : Board s -> Board s
stopAnimation board =
    let animation = board.animation
    in  { board | animation = { animation | t = 0, onDelta = always Sub.none }}

startAnimation : Board s -> Board s
startAnimation ( { animation } as board ) =
    { board | animation = { animation | t = 0, onDelta = onAnimationFrameDelta }}

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
            then Just ( HeroData x y Up noHeroAnimation [] )
            else Nothing

        hero = tilesWithIndex board
            |> List.filterMap startTileToHero
            |> List.head
            |> Maybe.withDefault ( HeroData 0 0 Up noHeroAnimation [] )
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

queryActor : ( Actor s -> Maybe a ) -> List ( Actor s ) -> Maybe a
queryActor get actors =
    actors
        |> List.filterMap get
        |> List.head

isRunning : List ( Actor s ) -> Maybe Bool
isRunning actors =
    let
        executorRunning actor =
            case actor of
                Executor executor -> Just ( executor.solver /= Nothing )
                _ -> Nothing
    in
        actors |> queryActor executorRunning

getMove : List ( Actor s ) -> Maybe Move
getMove actors =
    let
        move actor =
            case actor of
                Executor executor -> Just executor.move
                _ -> Nothing
    in
        actors |> queryActor move

getInput : List ( Actor s ) -> Maybe Move
getInput actors =
    let
        input actor =
            case actor of
                InputController move -> Just move
                _ -> Nothing

    in
        actors |> queryActor input

getHero : List ( Actor s ) -> Maybe HeroData
getHero actors =
    let
        hero actor =
            case actor of
                Hero heroData -> Just heroData
                _ -> Nothing

    in
        actors |> queryActor hero

viewActor : Float -> Float -> Actor s -> Collage Msg
viewActor t cellSize actor =
    case actor of
        Hero hero         -> viewHero t cellSize hero
        Friend friend     -> viewFriend t cellSize friend
        InputController _ -> Collage.group []
        Executor _        -> Collage.group []


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

viewFriend : Float -> Float -> FriendData -> Collage Msg
viewFriend t cellSize friend =
    let
        angle = case friend.phi of
            Left  -> pi/2
            Up    -> 0
            Right -> -pi/2
            Down  -> pi

        dPhi = friend.animation.dPhi t
        dX = friend.animation.dX t
        dY = friend.animation.dY t
    in
        [ Text.fromString "🦋"
            |> Text.size (round (cellSize/5*3))
            |> rendered
            |> rotate ( angle + dPhi)
        , circle (cellSize/5*2)
            |> filled transparent
        ]
        |> group
        |> shiftX ( cellSize * ( toFloat friend.x + dX ) )
        |> shiftY ( cellSize * ( toFloat friend.y + dY ) )

viewGame : Game solver -> List (Html Msg)
viewGame { board, programText } =
    let
        { actors, animation } = board
        cellSize = board.size / toFloat board.width

        viewActors = actors
            |> List.map ( viewActor animation.t cellSize )

        running = isRunning actors
            |> Maybe.withDefault False

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
                                [ Events.onFocus <| Coding True
                                , Events.onBlur <| Coding False
                                ]
                            ]
                        , if running
                            then Button.button
                                [ Button.danger
                                , Button.block
                                , Button.onClick StopRunning
                                ]
                                [ Html.text "Stop" ]
                            else Button.button
                                [ Button.outlineSuccess
                                , Button.block
                                , Button.onClick RunProgram
                                ]
                                [ Html.text "Go!" ]
                        , Button.button
                            [ Button.outlineWarning
                            , Button.block
                            , Button.disabled running
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
                _            -> KeyOtherDown string
    in
        Decode.field "key" Decode.string
            |> Decode.map toDirection

play : Configuration solver -> Program () (Game solver) Msg
play configuration =
    Browser.document
        { subscriptions =
            \{ board } -> Sub.batch
                [ onKeyDown keyDownDecoder
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

