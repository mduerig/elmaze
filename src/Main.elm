module Main exposing ( main )

import Game exposing
    ( Game, Board, newBoard, updateTileBoundary, updateTileType, updateTile, Boundary(..)
    , TileType(..), Msg, Direction(..), Move(..), updateTileBackground
    )
import Parse as P
import Interpreter as I

testBoard : Board
testBoard = newBoard 8 6
    |> updateTileBoundary (0, 0) Up Alley
    |> updateTileBoundary (0, 1) Up Alley
    |> updateTileBoundary (0, 2) Up Alley
    |> updateTileBoundary (0, 3) Right Alley
    |> updateTileBoundary (1, 3) Right Alley
    |> updateTileBoundary (2, 3) Right Alley
    |> updateTileBoundary (3, 3) Right Alley
    |> updateTileBoundary (4, 3) Down Alley
    |> updateTileBoundary (4, 2) Down Alley
    |> updateTileBoundary (4, 1) Left Alley
    |> updateTileBoundary (3, 1) Left Alley
    |> updateTileBoundary (2, 1) Left Alley
    |> updateTileBoundary (1, 1) Left Alley
    |> updateTileBoundary (1, 2) Right Alley
    |> updateTileBoundary (2, 2) Right Alley
    |> updateTileBoundary (4, 2) Right Alley
    |> updateTileBoundary (5, 2) Right Alley
    |> updateTileBoundary (6, 2) Down Alley
    |> updateTileBoundary (6, 1) Down Alley
    |> updateTileBoundary (6, 0) Left Alley
    |> updateTileBoundary (5, 0) Left Alley
    |> updateTileBoundary (4, 0) Up Alley
    |> updateTileBoundary (4, 3) Up Alley
    |> updateTileBoundary (4, 4) Right Alley
    |> updateTileBoundary (5, 4) Up Alley
    |> updateTileBoundary (5, 5) Left Alley
    |> updateTileBoundary (4, 5) Left Alley
    |> updateTileBoundary (3, 5) Left Alley
    |> updateTileBoundary (2, 5) Down Alley
    |> updateTileBoundary (2, 4) Down Alley
    |> updateTileBoundary (5, 5) Right Alley
    |> updateTileBoundary (6, 5) Right Alley
    |> updateTileBoundary (1, 0) Right Alley
    |> updateTileBoundary (2, 0) Right Alley
    |> updateTileBoundary (7, 0) Up Alley
    |> updateTileBoundary (7, 1) Up Alley
    |> updateTileBoundary (7, 2) Up Alley
    |> updateTileBoundary (7, 3) Up Alley
    |> updateTileBoundary (7, 3) Left Alley
    |> updateTileBoundary (6, 3) Up Alley
    |> updateTileBoundary (6, 3) Left Alley
    |> updateTileBoundary (7, 4) Left Alley
    |> updateTileBoundary (0, 4) Up Alley
    |> updateTileBoundary (0, 4) Right Alley
    |> updateTileBoundary (1, 4) Up Alley
    |> updateTileBoundary (1, 5) Left Alley
    |> updateTileBoundary (7, 4) Up Alley
    |> updateTileBoundary (7, 4) Left Wall
    |> updateTileBoundary (7, 4) Down Wall
    |> updateTile ( 0, 0 ) (updateTileType Start)
    |> updateTile ( 7, 4 ) (updateTileType Goal)
    |> updateTile ( 0, 0 ) ( updateTileBackground "tiles/ns.png")
    |> updateTile ( 0, 1 ) ( updateTileBackground "tiles/nes.png")
    |> updateTile ( 0, 2 ) ( updateTileBackground "tiles/ns.png")
    |> updateTile ( 0, 3 ) ( updateTileBackground "tiles/es.png")
    |> updateTile ( 1, 3 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 2, 3 ) ( updateTileBackground "tiles/new.png")
    |> updateTile ( 3, 3 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 4, 3 ) ( updateTileBackground "tiles/nsw.png")
    |> updateTile ( 1, 1 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 2, 1 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 3, 1 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 4, 1 ) ( updateTileBackground "tiles/nsw.png")
    |> updateTile ( 4, 0 ) ( updateTileBackground "tiles/ne.png")
    |> updateTile ( 5, 0 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 6, 0 ) ( updateTileBackground "tiles/nw.png")
    |> updateTile ( 6, 1 ) ( updateTileBackground "tiles/ns.png")
    |> updateTile ( 6, 2 ) ( updateTileBackground "tiles/sw.png")
    |> updateTile ( 5, 2 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 4, 2 ) ( updateTileBackground "tiles/nes.png")
    |> updateTile ( 2, 4 ) ( updateTileBackground "tiles/ns.png")
    |> updateTile ( 2, 5 ) ( updateTileBackground "tiles/es.png")
    |> updateTile ( 3, 5 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 4, 5 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 4, 4 ) ( updateTileBackground "tiles/es.png")
    |> updateTile ( 5, 5 ) ( updateTileBackground "tiles/esw.png")
    |> updateTile ( 5, 4 ) ( updateTileBackground "tiles/nw.png")
    |> updateTile ( 6, 5 ) ( updateTileBackground "tiles/ew.png")
    |> updateTile ( 7, 5 ) ( updateTileBackground "tiles/sw.png")
    |> updateTile ( 7, 4 ) ( updateTileBackground "tiles/ns.png")
    |> updateTile ( 1, 0 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 2, 0 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 3, 0 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 7, 0 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 7, 1 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 7, 2 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 7, 3 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 6, 3 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 5, 3 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 6, 4 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 5, 1 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 1, 2 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 2, 2 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 3, 2 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 0, 4 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 0, 5 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 1, 4 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 1, 5 ) ( updateTileBackground "tiles/0.png")
    |> updateTile ( 3, 4 ) ( updateTileBackground "tiles/0.png")

updateGame : Board -> Maybe I.Interpreter -> ( Maybe I.Interpreter, Move )
updateGame board interpreter =
    case interpreter of
        Just interp ->
            I.update board interp
                |> Tuple.mapFirst Just
        _ ->
            ( Nothing, Nop )

initGame : Board -> String -> Maybe I.Interpreter
initGame board program =
    case P.parse program of
        Ok ast ->
            Just <| I.init ast

        Err error ->
            Debug.log (Debug.toString error) Nothing

main : Program () (Game ( Maybe I.Interpreter )) Msg
main =
    Game.play
        { board = testBoard
        , init = initGame
        , update = updateGame
        }
