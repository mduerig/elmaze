module Main exposing ( main )

import Game exposing
    (Game, Board, newBoard, updateCellBoundary, updateCellType, updateCell, Boundary(..)
    , CellType(..), Msg, Direction(..), Move(..)
    )

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

updateGame : List String -> ( List String, Move )
updateGame commands =
    let
        newCommands =
            List.tail commands
                |> Maybe.withDefault []

        move =
            case List.head commands of
                Just "forward" -> Forward
                Just "right"   -> TurnRight
                Just "left"    -> TurnLeft
                _              -> Nop
    in
        ( newCommands, move )


main : Program () (Game ( List String ) ) Msg
main =
    Game.play
        { board = testBoard
        , init = .program >> String.split "\n"
        , update = updateGame
        }
