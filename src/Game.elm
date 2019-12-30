module Game exposing
    ( Game
    , Msg
    , Board
    , emptyBoard
    , Boundary ( .. )
    , withActor
    , withTileSet
    , withPath
    , fork2
    , fork3
    , deadEnd
    , withStartAt
    , withGoalAt
    , play
    )

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
import Actor as A exposing ( Actor )
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
    , tileSet : TileSet
    }

type alias Animation =
    { v : Float
    , t : Float
    , onDelta : (Float -> Msg) -> Sub Msg
    }

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
    | Path

type alias TileSet =
    Boundary -> Boundary -> Boundary -> Boundary -> Maybe String

type Path
    = DeadEnd
    | Segment (List A.Direction) Path
    | Fork Path Path

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
    | RandomDirection Actor A.Direction
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
                |> withController C.keyboardController
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
    , tileSet = always<| always <| always <| always Nothing
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
                        |> withController C.keyboardController
                  , programText = ""
                }
                , Cmd.none
                )

            Coding coding  ->
                ( { game | board = game.board
                    |> if coding
                        then withController C.nopController
                        else withController C.keyboardController
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
                        |> withController ( C.programController program )
                    }

            StopProgram ->
                updateGame StopInterpreter
                    { game
                    | board = board
                        |> withController C.keyboardController
                    }

            _ ->
                game
                    |> updateController msg
                    |> updateActors msg
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

isFreePredicate : Board -> A.IsFreePredicate
isFreePredicate board pos direction =
    queryTile pos board ( hasBoundary direction Path )

isGoalPredicate : Board -> A.IsGoalPredicate
isGoalPredicate board pos =
    queryTile pos board ( \tile -> tile.tileType == Goal )

updateActors : Msg -> Game -> ( Game, Msg )
updateActors msg ( { board } as game ) =
    let
        mapHero = updateHero msg ( isFreePredicate board ) ( isGoalPredicate board ) board.controller
        mapFriend = updateFriend msg ( isFreePredicate board )

        actorsAndMessages = A.mapActors mapHero mapFriend board.actors
    in
        ( { game | board = { board | actors = List.map Tuple.first actorsAndMessages } }
        , Batch ( List.map Tuple.second actorsAndMessages )
        )

updateController : Msg -> Game -> Game
updateController msg game =
    let
        update actor = game.board.controller
            |> case msg of
                KeyArrow direction -> C.updateKeyboardController direction
                StartInterpreter   -> C.updateProgramController ( isConditionTrue actor game.board )
                AnimationEnd       -> C.updateProgramController ( isConditionTrue actor game.board )
                _                  -> C.resetKeyboardController >> C.resetProgramController

        updatedController = game.board.actors
            |> A.filterActors ( always True ) ( always False )
            |> List.map ( update >> withController )
            |> headOrElse identity
    in
        { game | board = updatedController game.board }

headOrElse : a -> List a -> a
headOrElse default xs =
    xs
        |> List.head
        |> Maybe.withDefault default

withActor : Actor -> Board -> Board
withActor actor ( { actors } as board ) =
    { board | actors = actor :: actors }

withController : Controller -> Board -> Board
withController controller board =
    { board | controller = controller }

isConditionTrue : Actor -> Board -> P.Condition -> Bool
isConditionTrue hero board condition =
    case condition of
        P.Not notCondition
            -> not ( isConditionTrue hero board notCondition )

        P.Free
            -> A.canActorMove hero ( isFreePredicate board )

        P.Blocked
            -> not ( A.canActorMove hero ( isFreePredicate board ) )

        P.Goal
            -> A.isActorAtGoal hero ( isGoalPredicate board )

updateHero : Msg -> A.IsFreePredicate -> A.IsGoalPredicate -> Controller -> Actor -> ( Actor, Msg )
updateHero msg isFree isGoal controller actor =
    let
        running = C.isProgram controller

        recordMove direction =
            if running
                then nop
                else RecordMove ( A.directionToMove direction )
    in
        case C.getInput controller of
            A.Forward ->
                if A.canActorMove actor isFree then
                    ( A.moveActorAhead actor
                    , batch StartAnimation ( recordMove A.Up )
                    )
                else
                    ( A.playLoseAnimation actor
                    , batch StartAnimation StopProgram
                    )

            A.TurnLeft ->
                ( A.turnActor A.Left actor
                , batch StartAnimation ( recordMove A.Left )
                )

            A.TurnRight ->
                ( A.turnActor A.Right actor
                , batch StartAnimation ( recordMove A.Right )
                )

            _ ->
                if msg == AnimationEnd then
                    if A.isActorAtGoal actor isGoal then
                        ( A.playWinAnimation actor
                        , batch StopProgram ( if running then StartAnimation else nop )
                        )
                    else
                        ( A.clearActorAnimation actor
                        , nop
                        )
                else
                    ( actor, nop )

updateFriend : Msg -> A.IsFreePredicate -> Actor -> ( Actor, Msg )
updateFriend msg isFree actor =
    let
        rnd = Random.uniform A.Up [ A.Down, A.Left, A.Right ]
    in
        case msg of
            RandomDirection forActor direction ->
                if A.eqActor forActor actor
                    then ( A.setActorDirection direction actor, nop )
                    else ( actor, nop )

            AnimationStart ->
                if A.canActorMove actor isFree then
                    ( A.moveActorAhead actor, nop )
                else
                    ( actor, GenerateRandom ( Random.generate ( RandomDirection actor ) rnd ) )

            AnimationEnd ->
                ( A.clearActorAnimation actor, nop )

            _ ->
                ( actor, nop )

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

withStartAt : ( Int, Int ) -> Board -> Board
withStartAt ( x, y ) board =
    board
        |> updateTile ( x, y ) ( updateTileType Start )

withGoalAt : ( Int, Int ) -> Board -> Board
withGoalAt ( x, y ) board =
    board
        |> updateTile ( x, y ) ( updateTileType Goal )

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

updateTileBoundary : ( Int, Int ) -> A.Direction -> Boundary -> Board -> Board
updateTileBoundary ( x, y ) direction boundary board =
    let
        update dir tile =
            case dir of
                A.Up    -> { tile | top = boundary }
                A.Down  -> { tile | bottom = boundary }
                A.Left  -> { tile | left = boundary }
                A.Right -> { tile | right = boundary }
    in
        board
            |> updateTile ( x, y ) (update direction)
            |> updateTile ( neighbour (x, y) direction ) (update ( A.oppositeDirection direction ))

withTileSet : TileSet -> Board -> Board
withTileSet tileSet board =
    { board | tileSet = tileSet }

hasBoundary : A.Direction -> Boundary -> Tile -> Bool
hasBoundary direction boundary tile =
    boundary == case direction of
        A.Right -> tile.right
        A.Left  -> tile.left
        A.Up    -> tile.top
        A.Down  -> tile.bottom

withPath : ( Int, Int ) -> Path -> Board -> Board
withPath pos path board =
    case path of
        DeadEnd ->
            board

        Segment [] remainingPath ->
            board
                |> withPath pos remainingPath

        Segment ( direction::directions ) remainingPath ->
            board
                |> updateTileBoundary pos direction Path
                |> withPath
                    ( neighbour pos direction )
                    ( Segment directions remainingPath )

        Fork path1 path2 ->
            board
                |> withPath pos path1
                >> withPath pos path2

neighbour : ( Int, Int ) -> A.Direction -> ( Int, Int )
neighbour ( x, y ) direction =
    case direction of
        A.Up    -> ( x, y + 1 )
        A.Down  -> ( x, y - 1 )
        A.Left  -> ( x - 1, y )
        A.Right -> ( x + 1, y )

deadEnd : List A.Direction -> Path
deadEnd directions =
    Segment directions DeadEnd

fork2 : Path -> Path -> List A.Direction -> Path
fork2 outPath1 outPath2 inPath =
    Segment inPath ( Fork outPath1 outPath2 )

fork3 : Path -> Path -> Path -> List A.Direction -> Path
fork3 outPath1 outPath2 outPath3 inPath =
    Segment inPath ( Fork outPath1 ( Fork outPath2 outPath3 ) )

viewGame : Game -> List (Html Msg)
viewGame { board, programText } =
    let
        { actors, controller, animation } = board
        cellSize = board.size / toFloat board.width

        viewActors = actors
            |> List.map ( A.viewActor animation.t cellSize )

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
                            |> List.map  ( viewTile board.tileSet cellSize )
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

viewTile : TileSet -> Float -> ( Int, Int, Tile ) -> Collage Msg
viewTile tileSet size ( x, y, { top, left, bottom, right, tileType } ) =
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

        background = tileSet top right bottom left
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

