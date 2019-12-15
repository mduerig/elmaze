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
import Actor as A
import Interpreter exposing ( Interpreter )
import Parse as P

type alias Game =
    { board : Board
    , coding : Bool
    , programText : String
    }

type alias Board =
    { size : Float
    , width : Int
    , height : Int
    , tiles : Array Tile
    , actors : List Actor
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

type Actor
    = Hero ( A.ActorData Msg )
    | Friend ( A.ActorData Msg )
    | KbdInputController A.Move
    | PrgInputController ( Maybe Interpreter, A.Move )

type Msg
    = KeyArrow A.Direction
    | ResetGame
    | RunProgram
    | StopRunning
    | EnterMode
    | KeyOtherDown String
    | Resize Int Int
    | StartAnimation
    | RecordMove A.Move
    | AnimationFrame Float
    | AnimationStart
    | AnimationStep
    | AnimationEnd
    | ProgramChanged String
    | GotViewport ( Result Dom.Error Dom.Viewport )
    | Coding Bool
    | GenerateRandom ( Cmd Msg )
    | RandomDirection A.Direction

initGame : Board -> ( Game, Cmd Msg )
initGame board =
    let
        game =
            { board = board
                |> addActor
                    ( Hero
                        { x = 0
                        , y = 0
                        , phi = A.Up
                        , animation = A.noAnimation
                        , cmds = []
                        }
                    )
                |> addActor
                    ( Friend
                        { x = 1
                        , y = 1
                        , phi = A.Right
                        , animation = A.noAnimation
                        , cmds = []
                        }
                    )
                |> addActor ( PrgInputController ( Nothing, A.Nop ))
                |> addActor ( KbdInputController A.Nop )
            , coding = False
            , programText = ""
            }
    in
        ( game, getProgramTextareaWidth )

getProgramTextareaWidth : Cmd Msg
getProgramTextareaWidth =
    Task.attempt GotViewport ( Dom.getViewportOf "boardWidth" )

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
    , actors = []
    , animation =
        { v = 1.5
        , t = 0
        , onDelta = always Sub.none
        }
    }

updateGame : Msg -> Game -> ( Game, Cmd Msg )
updateGame msg  ( { board, programText } as game ) =
    let
        program =
            case P.parse ( ensureTrailingLF programText ) of
                Ok ast ->
                    Just <| Interpreter.init ast

                Err error ->
                    Debug.log (Debug.toString error) Nothing

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
                ( { game | programText = ensureTrailingLF programText ++ A.moveToString move }
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
                        |> initExecutor program
                    }

            StopRunning ->
                updateGame EnterMode
                    { game
                    | board = board
                        |> initExecutor Nothing
                    }

            _ ->
                game.board.actors
                    |> List.foldl ( updateActor msg ) game
                    |> runCommands

initExecutor : Maybe Interpreter -> Board -> Board
initExecutor interpreter board =
    board |>
        mapActors
            ( \actor ->
                case actor of
                    PrgInputController ( _, _ ) ->
                        PrgInputController ( interpreter, A.Nop )

                    _
                        -> actor
            )

runCommands : Game -> ( Game , Cmd Msg )
runCommands game =
    game.board.actors
        |> List.concatMap getCommands
        |> List.foldl runCommand ( game |> clearCommands, Cmd.none )

getCommands : Actor -> List Msg
getCommands actor =
    case actor of
        Hero hero            -> hero.cmds
        Friend friend        -> friend.cmds
        KbdInputController _ -> []
        PrgInputController _ -> []

runCommand : Msg -> ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
runCommand msg ( game, cmd ) =
    let
        ( newGame, newCommand ) = updateGame msg game
    in
        ( newGame, Cmd.batch [ cmd, newCommand ] )

updateActor : Msg -> Actor -> Game -> Game
updateActor msg actor game =
    let
        updatedActor =
            case actor of
                Hero hero ->
                    updateHero msg game hero

                Friend friend ->
                    updateFriend msg game friend

                KbdInputController _ ->
                    updateKbdInputController msg game

                PrgInputController ( Just interpreter, _ ) ->
                    updatePrgInputController msg game interpreter

                PrgInputController ( Nothing, _ ) ->
                    actor
    in
        { game | board = setActor updatedActor game.board }

addActor : Actor -> Board -> Board
addActor actor ( {actors } as board ) =
    { board | actors = actor :: actors }

mapActors : ( Actor -> Actor ) -> Board -> Board
mapActors update board =
    { board
    | actors = board.actors
        |> List.map update
    }

setActor : Actor -> Board -> Board
setActor actor board =
    board |> mapActors
        ( \currentActor ->
            case ( currentActor, actor ) of
                ( Hero _, Hero _ ) -> actor
                ( Friend _, Friend _ ) -> actor
                ( KbdInputController _, KbdInputController _ ) -> actor
                ( PrgInputController _, PrgInputController _ ) -> actor
                ( _, _ ) -> currentActor
        )

updateKbdInputController : Msg -> Game -> Actor
updateKbdInputController msg { coding } =
    if coding then
        KbdInputController A.Nop
    else
        case msg of
        KeyArrow A.Up    -> KbdInputController A.Forward
        KeyArrow A.Left  -> KbdInputController A.TurnLeft
        KeyArrow A.Right -> KbdInputController A.TurnRight
        _                -> KbdInputController A.Nop

isMet : Board -> P.Condition -> Bool
isMet board condition =
    let
        hero = getHero board.actors
            |> Maybe.withDefault ( A.ActorData 50 50 A.Up A.noAnimation [] )

        isAtGoal tile = tile.tileType == Goal

        queryHero : (Tile -> Bool) -> Bool
        queryHero = queryTile (hero.x, hero.y) board
    in
        case condition of
            P.Not notCondition
                -> not <| isMet board notCondition

            P.Free
                -> queryHero ( hasBoundary hero.phi Path )

            P.Blocked
                -> queryHero ( hasBoundary hero.phi Wall )

            P.Goal
                -> queryHero isAtGoal

updatePrgInputController : Msg -> Game -> Interpreter -> Actor
updatePrgInputController msg game interpreter =
    if msg == EnterMode || msg == AnimationEnd then
        PrgInputController <| Tuple.mapFirst Just ( Interpreter.update ( isMet game.board ) interpreter )
    else
        PrgInputController ( Just interpreter, A.Nop )

updateHero : Msg -> Game -> A.ActorData Msg -> Actor
updateHero msg { board } hero =
    let
        { x, y, phi } = hero

        isBlocked direction =
            queryTile ( x, y ) board ( hasBoundary direction Wall )

        atGoal = isHeroAtGoal hero board

        noAnimation = A.noAnimation
        winAnimation =
            { noAnimation
            | dY = Ease.outBack >> \t -> t
            , dPhi = \t -> 4 * pi * t
            }

        loseAnimation =
            { noAnimation
            | dY = Ease.inBack >> \t -> -10 * t
            , dPhi = \t -> 4 * pi * t
            }

        running = isRunning board.actors

        recordMove direction game =
            game |> if running
                then identity
                else A.sendCommand ( RecordMove ( A.directionToMove direction ))

        move =
            if running then
                getMove board.actors
            else
                getInput board.actors
    in
        case move of
            Just A.Forward ->
                if isBlocked phi then
                    hero
                        |> A.animate loseAnimation
                        |> A.sendCommand StartAnimation
                        |> A.sendCommand StopRunning
                        |> Hero
                else
                    hero
                        |> A.move phi
                        |> A.animate ( A.moveAnimation phi )
                        |> A.sendCommand StartAnimation
                        |> recordMove A.Up
                        |> Hero

            Just A.TurnLeft ->
                hero
                    |> A.turn A.Left
                    |> A.animate ( A.turnAnimation A.Left )
                    |> A.sendCommand StartAnimation
                    |> recordMove A.Left
                    |> Hero

            Just A.TurnRight ->
                hero
                    |> A.turn A.Right
                    |> A.animate ( A.turnAnimation A.Right )
                    |> A.sendCommand StartAnimation
                    |> recordMove A.Right
                    |> Hero

            _ ->
                if msg == AnimationEnd then
                    if atGoal then
                        hero
                            |> A.animate winAnimation
                            |> ( if running
                                    then A.sendCommand StartAnimation
                                    else identity
                               )
                            |> A.sendCommand StopRunning
                            |> Hero
                    else
                        hero
                            |> A.animate A.noAnimation
                            |> A.sendCommand StopRunning
                            |> Hero
                else
                    Hero hero

updateFriend : Msg -> Game -> A.ActorData Msg -> Actor
updateFriend msg { board } ( { x, y, phi } as friend ) =
    let
        isBlocked direction =
            queryTile ( x, y ) board ( hasBoundary direction Wall )

        rnd = Random.uniform A.Up [ A.Down, A.Left, A.Right ]
    in
        case msg of
            RandomDirection direction ->
                Friend { friend | phi = direction }

            AnimationStart ->
                if isBlocked phi then
                    Friend { friend | cmds = [ GenerateRandom <| Random.generate RandomDirection rnd ] }
                else
                    friend
                        |> A.move phi
                        |> A.animate ( A.moveAnimation phi )
                        |> Friend

            AnimationEnd ->
                friend
                    |> A.animate A.noAnimation
                    |> Friend

            _ ->
                Friend friend

clearCommands : Game -> Game
clearCommands ( { board } as game ) =
    let
        clearCmd actor = case actor of
            Hero hero            -> Hero { hero | cmds = [] }
            Friend friend        -> Friend { friend | cmds = [] }
            KbdInputController _ -> actor
            PrgInputController _ -> actor

    in
        { game | board = board |> mapActors clearCmd }

stopAnimation : Board -> Board
stopAnimation board =
    let animation = board.animation
    in  { board | animation = { animation | t = 0, onDelta = always Sub.none }}

startAnimation : Board -> Board
startAnimation ( { animation } as board ) =
    { board | animation = { animation | t = 0, onDelta = onAnimationFrameDelta }}

advanceAnimation : Float -> Board -> Board
advanceAnimation dt board =
    let animation = board.animation
    in  { board | animation = { animation | t = animation.t + animation.v * dt / 1000 }}

isHeroAtGoal : A.ActorData Msg -> Board -> Bool
isHeroAtGoal hero board =
    queryTile (hero.x, hero.y) board (\tile -> tile.tileType == Goal)

resetHero : Board -> Board
resetHero ( { actors } as board ) =
    let
        startTileToHero (x, y, tile) = if tile.tileType == Start
            then Just ( A.ActorData x y A.Up A.noAnimation [] )
            else Nothing

        hero = tilesWithIndex board
            |> List.filterMap startTileToHero
            |> List.head
            |> Maybe.withDefault ( A.ActorData 0 0 A.Up A.noAnimation [] )
    in
        { board | actors = setHero hero actors }

setHero : A.ActorData Msg -> List Actor -> List Actor
setHero heroData actors =
    let
        replaceHero actor =
            case actor of
                Hero _ -> Hero heroData
                _ -> actor
    in
        actors
            |> List.map replaceHero


queryTile : ( Int, Int ) -> Board -> (Tile -> Bool) -> Bool
queryTile ( x, y ) { width, height, tiles } query =
    Array.get ((height - 1 - y) * width + x) tiles
        |> Maybe.map query
        |> Maybe.withDefault True

updateTile : ( Int, Int ) -> (Tile -> Tile) -> Board -> Board
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

updateTileBoundary : ( Int, Int ) -> A.Direction -> Boundary -> Board -> Board
updateTileBoundary ( x, y ) direction boundary board =
    let
        update dir tile =
            case dir of
                A.Up    -> { tile | top = boundary }
                A.Down  -> { tile | bottom = boundary }
                A.Left  -> { tile | left = boundary }
                A.Right -> { tile | right = boundary }

        neighbour =
            case direction of
                A.Up    -> ( x, y + 1 )
                A.Down  -> ( x, y - 1 )
                A.Left  -> ( x - 1, y )
                A.Right -> ( x + 1, y )
    in
        board
            |> updateTile ( x, y ) (update direction)
            |> updateTile neighbour (update <| A.oppositeDirection direction)

hasBoundary : A.Direction -> Boundary -> Tile -> Bool
hasBoundary direction boundary tile =
    boundary == case direction of
        A.Right -> tile.right
        A.Left  -> tile.left
        A.Up    -> tile.top
        A.Down  -> tile.bottom

queryActor : ( Actor -> Maybe a ) -> List ( Actor ) -> Maybe a
queryActor get actors =
    actors
        |> List.filterMap get
        |> List.head

isRunning : List Actor -> Bool
isRunning actors =
    let
        executorRunning actor =
            case actor of
                PrgInputController ( interpreter, _ ) -> Just ( interpreter /= Nothing )
                _ -> Nothing
    in
        actors
            |> queryActor executorRunning
            |> Maybe.withDefault False

getMove : List Actor -> Maybe A.Move
getMove actors =
    let
        move actor =
            case actor of
                PrgInputController ( _ , prgMove ) -> Just prgMove
                _ -> Nothing
    in
        actors |> queryActor move

getInput : List Actor -> Maybe A.Move
getInput actors =
    let
        input actor =
            case actor of
                KbdInputController move -> Just move
                _ -> Nothing

    in
        actors |> queryActor input

getHero : List Actor -> Maybe ( A.ActorData Msg )
getHero actors =
    let
        hero actor =
            case actor of
                Hero heroData -> Just heroData
                _ -> Nothing

    in
        actors |> queryActor hero

viewActor : Float -> Float -> Actor -> Collage Msg
viewActor t cellSize actor =
    case actor of
        Hero hero            -> viewHero t cellSize hero
        Friend friend        -> viewFriend t cellSize friend
        KbdInputController _ -> Collage.group []
        PrgInputController _ -> Collage.group []

viewHero : Float -> Float -> A.ActorData Msg -> Collage Msg
viewHero t cellSize hero =
    let
        angle = case hero.phi of
            A.Left  -> pi/2
            A.Up    -> 0
            A.Right -> -pi/2
            A.Down  -> pi

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

viewFriend : Float -> Float -> A.ActorData Msg -> Collage Msg
viewFriend t cellSize friend =
    let
        angle = case friend.phi of
            A.Left  -> pi/2
            A.Up    -> 0
            A.Right -> -pi/2
            A.Down  -> pi

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

viewGame : Game -> List (Html Msg)
viewGame { board, programText } =
    let
        { actors, animation } = board
        cellSize = board.size / toFloat board.width

        viewActors = actors
            |> List.map ( viewActor animation.t cellSize )

        running = isRunning actors

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
                "ArrowLeft"  -> KeyArrow A.Left
                "ArrowRight" -> KeyArrow A.Right
                "ArrowUp"    -> KeyArrow A.Up
                "ArrowDown"  -> KeyArrow A.Down
                _            -> KeyOtherDown string
    in
        Decode.field "key" Decode.string
            |> Decode.map toDirection

play : Board -> Program () Game Msg
play board =
    Browser.document
        { subscriptions =
            \game -> Sub.batch
                [ onKeyDown keyDownDecoder
                , game.board.animation.onDelta AnimationFrame
                , onResize Resize
                ]
        , init = \_ -> initGame board
        , update = updateGame
        , view =
            \game ->
                { title = "Elmaze"
                , body = viewGame game
                }
        }

