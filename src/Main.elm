module Main exposing ( main )

import Game exposing
    ( Game, Board, newBoard, updateTileBoundary, updateTileType, updateTile, Boundary(..)
    , TileType(..), Msg, updateTileBackground, Actor
    )
import Actor as A
import Parse as P
import Interpreter as I

testBoard : Board s
testBoard = newBoard 8 6
    |> updateTileBoundary (0, 0) A.Up Path
    |> updateTileBoundary (0, 1) A.Up Path
    |> updateTileBoundary (0, 2) A.Up Path
    |> updateTileBoundary (0, 3) A.Right Path
    |> updateTileBoundary (1, 3) A.Right Path
    |> updateTileBoundary (2, 3) A.Right Path
    |> updateTileBoundary (3, 3) A.Right Path
    |> updateTileBoundary (4, 3) A.Down Path
    |> updateTileBoundary (4, 2) A.Down Path
    |> updateTileBoundary (4, 1) A.Left Path
    |> updateTileBoundary (3, 1) A.Left Path
    |> updateTileBoundary (2, 1) A.Left Path
    |> updateTileBoundary (1, 1) A.Left Path
    |> updateTileBoundary (1, 2) A.Right Path
    |> updateTileBoundary (2, 2) A.Right Path
    |> updateTileBoundary (4, 2) A.Right Path
    |> updateTileBoundary (5, 2) A.Right Path
    |> updateTileBoundary (6, 2) A.Down Path
    |> updateTileBoundary (6, 1) A.Down Path
    |> updateTileBoundary (6, 0) A.Left Path
    |> updateTileBoundary (5, 0) A.Left Path
    |> updateTileBoundary (4, 0) A.Up Path
    |> updateTileBoundary (4, 3) A.Up Path
    |> updateTileBoundary (4, 4) A.Right Path
    |> updateTileBoundary (5, 4) A.Up Path
    |> updateTileBoundary (5, 5) A.Left Path
    |> updateTileBoundary (4, 5) A.Left Path
    |> updateTileBoundary (3, 5) A.Left Path
    |> updateTileBoundary (2, 5) A.Down Path
    |> updateTileBoundary (2, 4) A.Down Path
    |> updateTileBoundary (5, 5) A.Right Path
    |> updateTileBoundary (6, 5) A.Right Path
    |> updateTileBoundary (1, 0) A.Right Path
    |> updateTileBoundary (2, 0) A.Right Path
    |> updateTileBoundary (7, 0) A.Up Path
    |> updateTileBoundary (7, 1) A.Up Path
    |> updateTileBoundary (7, 2) A.Up Path
    |> updateTileBoundary (7, 3) A.Up Path
    |> updateTileBoundary (7, 3) A.Left Path
    |> updateTileBoundary (6, 3) A.Up Path
    |> updateTileBoundary (6, 3) A.Left Path
    |> updateTileBoundary (7, 4) A.Left Path
    |> updateTileBoundary (0, 4) A.Up Path
    |> updateTileBoundary (0, 4) A.Right Path
    |> updateTileBoundary (1, 4) A.Up Path
    |> updateTileBoundary (1, 5) A.Left Path
    |> updateTileBoundary (7, 4) A.Up Path
    |> updateTileBoundary (7, 4) A.Left Wall
    |> updateTileBoundary (7, 4) A.Down Wall
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

isMet : Game.Board s -> P.Condition -> Bool
isMet board condition =
    let
        hero = Game.getHero board.actors
            |> Maybe.withDefault ( A.ActorData 50 50 A.Up A.noAnimation [] )

        isAtGoal tile = tile.tileType == Game.Goal

        queryHero : (Game.Tile -> Bool) -> Bool
        queryHero = Game.queryTile (hero.x, hero.y) board
    in
        case condition of
            P.Not notCondition
                -> not <| isMet board notCondition

            P.Free
                -> queryHero ( Game.hasBoundary hero.phi Game.Path )

            P.Blocked
                -> queryHero ( Game.hasBoundary hero.phi Game.Wall )

            P.Goal
                -> queryHero isAtGoal

updateGame : Board s -> Maybe I.Interpreter -> ( Maybe I.Interpreter, A.Move )
updateGame board interpreter =
    case interpreter of
        Just interp ->
            I.update ( isMet board ) interp
                |> Tuple.mapFirst Just
        _ ->
            ( Nothing, A.Nop )

initGame : Board s -> String -> Maybe I.Interpreter
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
