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
    -- |> updateTile ( 0, 0 ) ( updateTileBackground "vert.png")
    -- |> updateTile ( 1, 1 ) ( updateTileBackground "hor.png")

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
