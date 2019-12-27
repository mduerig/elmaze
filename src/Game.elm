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
import Actor as A
import Controller as C exposing ( Controller )
import Interpreter
import Parse as P

type alias Game =
    { board : Board
    , programText : String
    }

type alias Board =
    { size : Float
    , width : Int
    , height : Int
    , tiles : Array Tile
    , defaultActors : List Actor
    , controller : Controller
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
    = Hero A.ActorData
    | Friend A.ActorData

type Msg
    = KeyArrow A.Direction
    | ResetGame
    | StartProgram
    | StopProgram
    | StartInterpreter
    | StopInterpreter
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
    | Batch ( List Msg )

nop : Msg
nop = Batch []

batch : Msg -> Msg -> Msg
batch msg1 msg2 =
    case ( msg1, msg2 ) of
       ( Batch msgs1, Batch msgs2 ) -> Batch ( msgs1 ++ msgs2 )
       ( Batch msgs, _ )            -> Batch ( msgs ++ [ msg2 ] )
       ( _, Batch msgs )            -> Batch ( msg1 :: msgs )
       _                            -> Batch [ msg1, msg2 ]

initGame : Board -> flags -> ( Game, Cmd Msg )
initGame board _ =
    let
        game =
            { board = { board
                | defaultActors = board.actors }
                |> setController C.keyboardController
            , programText = ""
            }
    in
        ( game, getProgramTextareaWidth )

getProgramTextareaWidth : Cmd Msg
getProgramTextareaWidth =
    Task.attempt GotViewport ( Dom.getViewportOf "boardWidth" )

emptyBoard : Int -> Int -> Board
emptyBoard width height =
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
    , defaultActors = []
    , controller = C.keyboardController
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
                Ok ast    -> Interpreter.init ast
                Err error -> Debug.log (Debug.toString error) Interpreter.void

    in
        case msg of
            Batch msgs ->
                msgs
                    |> List.foldl runCommand ( game, Cmd.none )

            GenerateRandom cmd ->
                ( game, cmd )

            ResetGame ->
                ( { game
                  | board = board
                        |> resetActors
                        |> setController C.keyboardController
                  , programText = ""
                }
                , Cmd.none
                )

            Coding coding  ->
                ( { game | board = game.board
                    |> if coding
                        then setController C.nopController
                        else setController C.keyboardController
                  }
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

            StartProgram ->
                updateGame StartInterpreter
                    { game
                    | board = board
                        |> resetActors
                        |> setController ( C.programController program )
                    }

            StopProgram ->
                updateGame StopInterpreter
                    { game
                    | board = board
                        |> setController C.keyboardController
                    }

            _ ->
                game.board.actors
                    |> List.foldl ( updateActor msg ) ( updateController msg game, nop )
                    |> runCommands

runCommands : ( Game, Msg ) -> ( Game, Cmd Msg )
runCommands ( game, cmd ) =
    updateGame cmd game

runCommand : Msg -> ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
runCommand msg ( game, cmd ) =
    let
        ( newGame, newCommand ) = updateGame msg game
    in
        ( newGame, Cmd.batch [ cmd, newCommand ] )

updateActor : Msg -> Actor -> ( Game, Msg ) -> ( Game, Msg )
updateActor msg actor ( game, cmds ) =
    let
        ( updatedActor, cmd ) =
            case actor of
                Hero hero ->
                    updateHero msg game hero

                Friend friend ->
                    updateFriend msg game friend
    in
        ( { game | board = setActor updatedActor game.board }
        , batch cmd cmds
        )

updateController : Msg -> Game -> Game
updateController msg game =
    let
        updatedController = game.board.controller
            |> case msg of
                KeyArrow direction -> C.updateKeyboardController direction
                StartInterpreter   -> C.updateProgramController ( isConditionTrue game.board )
                AnimationEnd       -> C.updateProgramController ( isConditionTrue game.board )
                _                  -> C.resetKeyboardController >> C.resetProgramController
    in
        { game | board = setController updatedController game.board }

addActor : Actor -> Board -> Board
addActor actor ( { actors } as board ) =
    { board | actors = actor :: actors }

setController : Controller -> Board -> Board
setController controller board =
    { board | controller = controller }

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
                ( _, _ ) -> currentActor
        )

isConditionTrue : Board -> P.Condition -> Bool
isConditionTrue board condition =
    case condition of
        P.Not notCondition
            -> not <| isConditionTrue board notCondition

        P.Free
            -> canHeroMove board

        P.Blocked
            -> not ( canHeroMove board )

        P.Goal
            -> isHeroAtGoal board

updateHero : Msg -> Game -> A.ActorData -> ( Actor, Msg )
updateHero msg { board } hero =
    let
        isBlocked direction =
            queryTile ( hero.x, hero.y ) board ( hasBoundary direction Wall )

        isAtGoal =
            queryTile ( hero.x, hero.y ) board ( \tile -> tile.tileType == Goal )

        running = C.isProgram board.controller

        recordMove direction =
            if running
                then nop
                else RecordMove ( A.directionToMove direction )
    in
        case C.getInput board.controller of
            A.Forward ->
                if isBlocked hero.phi then
                    ( hero
                        |> A.animate A.loseAnimation
                        |> Hero
                    , batch StartAnimation StopProgram
                    )
                else
                    ( hero
                        |> A.move hero.phi
                        |> A.animate ( A.moveAnimation hero.phi )
                        |> Hero
                    , batch StartAnimation ( recordMove A.Up )
                    )

            A.TurnLeft ->
                ( hero
                    |> A.turn A.Left
                    |> A.animate ( A.turnAnimation A.Left )
                    |> Hero
                , batch StartAnimation ( recordMove A.Left )
                )

            A.TurnRight ->
                ( hero
                    |> A.turn A.Right
                    |> A.animate ( A.turnAnimation A.Right )
                    |> Hero
                , batch StartAnimation ( recordMove A.Right )
                )

            _ ->
                if msg == AnimationEnd then
                    if isAtGoal then
                        ( hero
                            |> A.animate A.winAnimation
                            |> Hero
                        , batch StopProgram ( if running then StartAnimation else nop )
                        )
                    else
                        ( hero
                            |> A.animate A.noAnimation
                            |> Hero
                        , nop
                        )
                else
                    ( Hero hero, nop )

updateFriend : Msg -> Game -> A.ActorData -> ( Actor, Msg )
updateFriend msg { board } ( { x, y, phi } as friend ) =
    let
        isBlocked direction =
            queryTile ( x, y ) board ( hasBoundary direction Wall )

        rnd = Random.uniform A.Up [ A.Down, A.Left, A.Right ]
    in
        case msg of
            RandomDirection direction ->
                ( Friend { friend | phi = direction }
                , nop
                )

            AnimationStart ->
                if isBlocked phi then
                    ( Friend friend
                    , GenerateRandom ( Random.generate RandomDirection rnd )
                    )
                else
                    ( friend
                        |> A.move phi
                        |> A.animate ( A.moveAnimation phi )
                        |> Friend
                    , nop
                    )

            AnimationEnd ->
                ( friend
                    |> A.animate A.noAnimation
                    |> Friend
                , nop
                )

            _ ->
                ( Friend friend, nop )

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

resetActors : Board -> Board
resetActors board =
    { board | actors = board.defaultActors }

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

canHeroMove : Board -> Bool
canHeroMove board =
    let
        canMove actor =
            case actor of
               Hero hero -> Just ( queryTile ( hero.x, hero.y ) board ( hasBoundary hero.phi Path ) )
               _ -> Nothing
    in
        board.actors
            |> queryActor canMove
            |> Maybe.withDefault False

isHeroAtGoal : Board -> Bool
isHeroAtGoal board =
    let
        atGoal actor =
            case actor of
               Hero hero -> Just ( queryTile ( hero.x, hero.y ) board (\tile -> tile.tileType == Goal) )
               _ -> Nothing
    in
        board.actors
            |> queryActor atGoal
            |> Maybe.withDefault False

viewActor : Float -> Float -> Actor -> Collage Msg
viewActor t cellSize actor =
    case actor of
        Hero actorData    -> A.viewActor t cellSize actorData
        Friend actorData  -> A.viewActor t cellSize actorData

viewGame : Game -> List (Html Msg)
viewGame { board, programText } =
    let
        { actors, controller, animation } = board
        cellSize = board.size / toFloat board.width

        viewActors = actors
            |> List.map ( viewActor animation.t cellSize )

        running = C.isProgram controller

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
                                , Button.onClick StopProgram
                                ]
                                [ Html.text "Stop" ]
                            else Button.button
                                [ Button.outlineSuccess
                                , Button.block
                                , Button.onClick StartProgram
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
        , init = initGame board
        , update = updateGame
        , view =
            \game ->
                { title = "Elmaze"
                , body = viewGame game
                }
        }

