module Main exposing ( main )

import Game exposing
    ( Game, Board, newBoard, updateCellBoundary, updateCellType, updateCell, Boundary(..)
    , CellType(..), Msg, Direction(..), Move(..), Move(..)
    )
import Parse as P
import Parser
import Interpreter as I

type alias RunProgram =
    { board : Board
    , interpreter : Maybe I.Interpreter
    }

testBoard : Board
testBoard = newBoard 4 4
    |> updateCellBoundary (0, 0) Up Alley
    |> updateCellBoundary (0, 1) Up Alley
    |> updateCellBoundary (0, 2) Up Alley
    |> updateCellBoundary (0, 3) Right Alley
    |> updateCellBoundary (1, 3) Right Alley
    |> updateCellBoundary (2, 3) Right Alley
    |> updateCellBoundary (3, 3) Down Alley
    |> updateCellBoundary (3, 2) Down Alley
    |> updateCellBoundary (3, 1) Left Alley
    |> updateCellBoundary (2, 1) Left Alley
    |> updateCellBoundary (1, 1) Left Alley
    |> updateCell ( 0, 0 ) (updateCellType Start)
    |> updateCell ( 3, 3 ) (updateCellType Goal)

updateGame : RunProgram -> ( RunProgram, Move )
updateGame runProgram =
    let
        { board, interpreter } = runProgram

        ( newInterpreter, move ) =
            case interpreter of
                Just interp
                    -> I.update board interp
                        |> Tuple.mapFirst Just
                _
                    ->  ( Nothing, Nop )
    in
        ( { runProgram | interpreter = newInterpreter }, move )

initGame : Board -> String -> RunProgram
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
        { board = board, interpreter = interpreter}

main : Program () (Game RunProgram ) Msg
main =
    Game.play
        { board = testBoard
        , init = initGame
        , update = updateGame
        }
