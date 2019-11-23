module Main exposing ( main )

import Game exposing
    ( Game, Board, newBoard, updateTileBoundary, updateTileType, updateTile, Boundary(..)
    , TileType(..), Msg, Direction(..), Move(..)
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
    |> updateTile ( 0, 0 ) (updateTileType Start)
    |> updateTile ( 3, 3 ) (updateTileType Goal)

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
