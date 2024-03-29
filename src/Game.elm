module Game exposing
    ( Game
    , Msg
    , Level
    , Board
    , Path
    , TileSet
    , emptyBoard
    , emptyLevel
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
import Info exposing ( Info )
import MenuBar exposing ( MenuBar )

type alias Game =
    { levels : List Level
    , board : Board
    , title : String
    , info : Info
    , menuBar : MenuBar
    , programText : String
    }

type alias Level =
    { title : String
    , board : Board
    , programText : String
    , infoTitle : List ( Html Info.Msg )
    , infoText : List ( Html Info.Msg )
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
    | Segment ( List A.Direction ) Path
    | Fork Path Path

type Msg
    = KeyArrow A.Direction
    | InfoMsg Info.Msg
    | MenuBarChange MenuBar
    | ShowInfo
    | SelectLevel Level
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
       ( Batch msgs, _ )            -> Batch ( msgs ++ [ msg2 ])
       ( _, Batch msgs )            -> Batch ( msg1 :: msgs )
       _                            -> Batch [ msg1, msg2 ]

initGame : List Level -> flags -> ( Game, Cmd Msg )
initGame levels _ =
    let
        { board, programText, title, infoTitle, infoText } = levels
            |> List.head
            |> Maybe.withDefault emptyLevel

        ( menuBar, menuBarCmd ) = MenuBar.init title MenuBarChange

        game =
            { levels = levels
            , board = { board | defaultActors = board.actors }
                |> withController C.keyboardController
            , title = title
            , info = Info.init True infoTitle infoText
            , menuBar = menuBar
            , programText = programText
            }
    in
        ( game, Cmd.batch [ getProgramTextareaWidth, menuBarCmd ])

emptyLevel : Level
emptyLevel =
    { title = "No levels to play"
    , board = emptyBoard 0 0
    , programText = ""
    , infoTitle = [ Html.text "No levels" ]
    , infoText = [ Html.text "This game has no levels" ]
    }

setLevel : Level -> Game -> Game
setLevel { board, programText, title, infoTitle, infoText } game =
    { game
        | board = { board | defaultActors = board.actors }
            |> withController C.keyboardController
        , title = title
        , info = Info.init True infoTitle infoText
        , menuBar = game.menuBar
            |> MenuBar.withLevelToggle title
        , programText = programText
    }

getProgramTextareaWidth : Cmd Msg
getProgramTextareaWidth =
    Task.attempt GotViewport ( Dom.getViewportOf "boardWidth" )

emptyBoard : Int -> Int -> Board
emptyBoard width height =
    { size = 500
    , width = width
    , height = height
    , tiles =
        Array.repeat ( width * height )
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
updateGame msg  ( { board, info, programText } as game ) =
    let
        program =
            case P.parse ( ensureTrailingLF programText ) of
                Ok ast    -> Interpreter.init ast
                Err error -> Debug.log ( Debug.toString error ) Interpreter.void

    in
        case msg of
            Batch msgs ->
                msgs
                    |> List.foldl runCommand ( game, Cmd.none )

            GenerateRandom cmd ->
                ( game, cmd )

            InfoMsg infoMsg ->
                ( { game | info = Info.update infoMsg info }
                , Cmd.none
                )

            MenuBarChange menuBar ->
                ( { game | menuBar = menuBar }
                , Cmd.none
                )

            ResetGame ->
                ( { game
                  | board = board
                        |> resetActors
                        |> withController C.keyboardController
                  , programText = ""
                  }
                , Cmd.none
                )

            ShowInfo ->
                ( { game | info = Info.show info }
                , Cmd.none
                )

            SelectLevel level ->
                ( game |> setLevel level
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
        ( { game | board = { board | actors = List.map Tuple.first actorsAndMessages }}
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
headOrElse default =
    List.head >> Maybe.withDefault default

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
            -> not ( A.canActorMove hero ( isFreePredicate board ))

        P.Goal
            -> A.isActorAtGoal hero ( isGoalPredicate board )

updateHero : Msg -> A.IsFreePredicate -> A.IsGoalPredicate -> Controller -> Actor -> ( Actor, Msg )
updateHero msg isFree isGoal controller hero =
    let
        running = C.isProgram controller

        recordMove direction =
            if running
                then nop
                else RecordMove ( A.directionToMove direction )
    in
        case C.getInput controller of
            A.Forward ->
                if A.canActorMove hero isFree then
                    ( A.moveActorAhead hero
                    , batch StartAnimation ( recordMove A.Up )
                    )
                else
                    ( A.playLoseAnimation hero
                    , batch StartAnimation StopProgram
                    )

            A.TurnLeft ->
                ( A.turnActor A.Left hero
                , batch StartAnimation ( recordMove A.Left )
                )

            A.TurnRight ->
                ( A.turnActor A.Right hero
                , batch StartAnimation ( recordMove A.Right )
                )

            _ ->
                if msg == AnimationEnd then
                    if A.isActorAtGoal hero isGoal then
                        ( A.playWinAnimation hero
                        , batch StopProgram ( if running then StartAnimation else nop )
                        )
                    else
                        ( A.clearActorAnimation hero
                        , nop
                        )
                else
                    ( hero, nop )

updateFriend : Msg -> A.IsFreePredicate -> Actor -> ( Actor, Msg )
updateFriend msg isFree friend =
    let
        rnd = Random.uniform A.Up [ A.Down, A.Left, A.Right ]
    in
        case msg of
            RandomDirection actor direction ->
                if A.eqActor actor friend
                    then ( A.setActorDirection direction friend, nop )
                    else ( friend, nop )

            AnimationStart ->
                if A.canActorMove friend isFree then
                    ( A.moveActorAhead friend, nop )
                else
                    ( friend, GenerateRandom ( Random.generate ( RandomDirection friend ) rnd ))

            AnimationEnd ->
                ( A.clearActorAnimation friend, nop )

            _ ->
                ( friend, nop )

stopAnimation : Board -> Board
stopAnimation ( { animation } as board ) =
    { board | animation = { animation | t = 0, onDelta = always Sub.none }}

startAnimation : Board -> Board
startAnimation ( { animation } as board ) =
    { board | animation = { animation | t = 0, onDelta = onAnimationFrameDelta }}

advanceAnimation : Float -> Board -> Board
advanceAnimation dt ( { animation } as board ) =
    { board | animation = { animation | t = animation.t + animation.v * dt / 1000 }}

resetActors : Board -> Board
resetActors board =
    { board | actors = board.defaultActors }

queryTile : ( Int, Int ) -> Board -> (Tile -> Bool) -> Bool
queryTile ( x, y ) { width, height, tiles } query =
    Array.get (( height - 1 - y)  * width + x ) tiles
        |> Maybe.map query
        |> Maybe.withDefault True

withStartAt : ( Int, Int ) -> Board -> Board
withStartAt ( x, y ) =
    updateTile ( x, y ) ( withTileType Start )

withGoalAt : ( Int, Int ) -> Board -> Board
withGoalAt ( x, y ) =
    updateTile ( x, y ) ( withTileType Goal )

updateTile : ( Int, Int ) -> ( Tile -> Tile ) -> Board -> Board
updateTile ( x, y ) update ( { width, height, tiles } as board ) =
    let
        i = ( height - 1 - y ) * width + x
        set tile = { board | tiles = Array.set i tile tiles }
    in
        Array.get i tiles
            |> Maybe.map ( update >> set )
            |> Maybe.withDefault board

withTileType : TileType -> Tile -> Tile
withTileType tileType tile = { tile | tileType = tileType }

withTileBoundary : ( Int, Int ) -> A.Direction -> Boundary -> Board -> Board
withTileBoundary ( x, y ) direction boundary =
    let
        withBoundary dir tile =
            case dir of
                A.Up    -> { tile | top = boundary }
                A.Down  -> { tile | bottom = boundary }
                A.Left  -> { tile | left = boundary }
                A.Right -> { tile | right = boundary }
    in
           updateTile ( x, y ) ( withBoundary direction )
        >> updateTile ( A.neighbour (x, y) direction ) ( withBoundary ( A.oppositeDirection direction ))

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

        Segment ( direction :: directions ) remainingPath ->
            board
                |> withTileBoundary pos direction Path
                |> withPath
                    ( A.neighbour pos direction )
                    ( Segment directions remainingPath )

        Fork path1 path2 ->
            board
                |> withPath pos path1
                >> withPath pos path2

deadEnd : List A.Direction -> Path
deadEnd directions =
    Segment directions DeadEnd

fork2 : Path -> Path -> List A.Direction -> Path
fork2 outPath1 outPath2 inPath =
    Segment inPath ( Fork outPath1 outPath2 )

fork3 : Path -> Path -> Path -> List A.Direction -> Path
fork3 outPath1 outPath2 outPath3 inPath =
    Segment inPath ( Fork outPath1 ( Fork outPath2 outPath3 ) )

levelItem : Level -> MenuBar.LevelItem Msg
levelItem level =
    { text = level.title
    , onSelect = SelectLevel level
    }

viewGame : Game -> List (Html Msg)
viewGame { levels, board, title, info, menuBar, programText } =
    let
        cellSize = board.size / toFloat board.width
        boardWidth = cellSize * toFloat board.width
        boardHeight = cellSize * toFloat board.height

        viewActors = board.actors
            |> List.map ( A.viewActor board.animation.t cellSize )

        running = C.isProgram board.controller
    in
        [ CDN.stylesheet
        , MenuBar.view MenuBarChange ShowInfo ( List.map levelItem levels ) menuBar
        , Grid.containerFluid []
            [ Grid.row []
                [ Grid.col [] []
                , Grid.col []
                    [   viewActors
                    ++  ( tilesWithIndex board
                            |> List.map  ( viewTile board.tileSet cellSize )
                        )
                    ++  viewBackground boardWidth boardHeight cellSize title
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
            , Info.view info
                |> Html.map InfoMsg
            ]
        ]

viewBackground : Float -> Float -> Float -> String -> List ( Collage Msg )
viewBackground boardWidth boardHeight cellSize title =
    [ Text.fromString title
        |> Text.size ( floor ( cellSize / 2 ) )
        |> rendered
        |> shiftX (( boardWidth - cellSize ) / 2 )
        |> shiftY boardHeight
    , rectangle ( 2.1 * cellSize + boardWidth ) ( 2.1 * cellSize + boardHeight )
        |> filled ( uniform Color.lightGray )
        |> shiftX (( boardWidth - cellSize ) / 2 )
        |> shiftY (( boardHeight - cellSize ) / 2 )
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
                    Text.fromString "🌺"
                       |> Text.size ( round ( size / 5 * 3 ))
                       |> rendered


                Start ->
                    Text.fromString "🌟"
                       |> Text.size ( round ( size / 5 * 4 ))
                       |> rendered
                       |> shiftY ( -size / 5 )

                _ ->
                    group []

        wallStyle wall =
            case wall of
                Wall  -> solid thin ( uniform Color.black )
                Path  -> invisible

        defaultTile = group
            [ square size
                |> filled ( uniform Color.lightYellow )
            , line size
                |> traced ( wallStyle bottom )
                |> shiftY ( -size / 2 )
            , line size
                |> traced ( wallStyle top )
                |> shiftY ( size / 2 )
            , line size
                |> traced ( wallStyle right )
                |> rotate ( pi / 2 )
                |> shiftX ( size / 2 )
            , line size
                |> traced ( wallStyle left )
                |> rotate ( pi / 2 )
                |> shiftX ( -size / 2 )
            ]
    in
        group
            [ tile
            , tileSet top right bottom left
                |> Maybe.map ( image ( size, size ) )
                |> Maybe.withDefault defaultTile
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

play : List Level -> Program () Game Msg
play levels =
    Browser.document
        { subscriptions = \game -> Sub.batch
            [ onKeyDown keyDownDecoder
            , game.board.animation.onDelta AnimationFrame
            , onResize Resize
            , Info.subscriptions game.info
                |> Sub.map InfoMsg
            , MenuBar.subscriptions MenuBarChange game.menuBar
            ]
        , init = initGame levels
        , update = updateGame
        , view = \game ->
            { title = game.title
            , body = viewGame game
            }
        }

