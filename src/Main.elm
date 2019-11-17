module Main exposing ( main )

import Game exposing
    ( Game, Board, newBoard, updateCellBoundary, updateCellType, updateCell, Boundary(..)
    , CellType(..), Msg, Direction(..), Move(..)
    )
import Parse as P
import Parser
import Interpreter as I

testBoard : Board
testBoard = newBoard 5 4
    |> updateCellBoundary (0, 0) Up Alley
    |> updateCellBoundary (0, 1) Up Alley
    |> updateCellBoundary (0, 2) Up Alley
    |> updateCellBoundary (0, 3) Right Alley
    |> updateCellBoundary (1, 3) Right Alley
    |> updateCellBoundary (2, 3) Right Alley
    |> updateCellBoundary (3, 3) Right Alley
    |> updateCellBoundary (4, 3) Down Alley
    |> updateCellBoundary (4, 2) Down Alley
    |> updateCellBoundary (4, 1) Left Alley
    |> updateCellBoundary (3, 1) Left Alley
    |> updateCellBoundary (2, 1) Left Alley
    |> updateCellBoundary (1, 1) Left Alley
    |> updateCell ( 0, 0 ) (updateCellType Start)
    |> updateCell ( 3, 3 ) (updateCellType Goal)

updateGame : Board -> Maybe I.Interpreter -> ( Maybe I.Interpreter, Move )
updateGame board interpreter =
    let
        ( newInterpreter, move ) =
            case interpreter of
                Just interp
                    -> I.update board interp
                        |> Tuple.mapFirst Just
                _
                    ->  ( Nothing, Nop )
    in
        ( newInterpreter, move )

initGame : Board -> String -> Maybe I.Interpreter
initGame board program =
    let
        parsed : Result (List Parser.DeadEnd) P.Program
        parsed = Parser.run P.program program

        interpreter : Maybe I.Interpreter
        interpreter = case parsed of
            Ok ast
                -> Just <| I.init ast
            Err error
                -> Debug.log (Debug.toString error) Nothing
    in
        interpreter

main : Program () (Game ( Maybe I.Interpreter )) Msg
main =
    Game.play
        { board = testBoard
        , init = initGame
        , update = updateGame
        }
