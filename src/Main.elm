module Main exposing ( main )

import Game exposing
    ( Game, Board, emptyBoard, updateTileBoundary, updateTileType, updateTile, Boundary(..)
    , TileType(..), Msg, addActor, setTileSet
    )
import Actor as A

testBoard : Board
testBoard = emptyBoard 8 6
    |> updateTileBoundary (0, 0) A.Up Path
    |> updateTileBoundary (0, 1) A.Up Path
    |> updateTileBoundary (0, 2) A.Up Path
    |> updateTileBoundary (0, 3) A.Right Path
    |> updateTileBoundary (1, 1) A.Left Path
    |> updateTileBoundary (1, 3) A.Right Path
    |> updateTileBoundary (2, 1) A.Left Path
    |> updateTileBoundary (2, 3) A.Right Path
    |> updateTileBoundary (2, 4) A.Down Path
    |> updateTileBoundary (2, 5) A.Down Path
    |> updateTileBoundary (3, 1) A.Left Path
    |> updateTileBoundary (3, 3) A.Right Path
    |> updateTileBoundary (3, 5) A.Left Path
    |> updateTileBoundary (4, 0) A.Up Path
    |> updateTileBoundary (4, 1) A.Left Path
    |> updateTileBoundary (4, 2) A.Down Path
    |> updateTileBoundary (4, 2) A.Right Path
    |> updateTileBoundary (4, 3) A.Down Path
    |> updateTileBoundary (4, 3) A.Up Path
    |> updateTileBoundary (4, 4) A.Right Path
    |> updateTileBoundary (4, 5) A.Left Path
    |> updateTileBoundary (5, 0) A.Left Path
    |> updateTileBoundary (5, 2) A.Right Path
    |> updateTileBoundary (5, 4) A.Up Path
    |> updateTileBoundary (5, 5) A.Left Path
    |> updateTileBoundary (5, 5) A.Right Path
    |> updateTileBoundary (6, 0) A.Left Path
    |> updateTileBoundary (6, 1) A.Down Path
    |> updateTileBoundary (6, 2) A.Down Path
    |> updateTileBoundary (6, 5) A.Right Path
    |> updateTileBoundary (7, 4) A.Up Path
    |> updateTile ( 0, 0 ) (updateTileType Start)
    |> updateTile ( 7, 4 ) (updateTileType Goal)
    |> addActor ( A.hero 0 ( 0, 0 ) A.Up "🐞" )
    |> addActor ( A.friend 1 ( 1, 1 ) A.Up  "🦋" )
    |> setTileSet
        ( \north east south west -> case [ north, east, south, west ] of
            [ Wall, Wall, Wall, Wall ] -> Just "tiles/0.png"
            [ Path, Wall, Wall, Wall ] -> Just "tiles/n.png"
            [ Wall, Path, Wall, Wall ] -> Just "tiles/e.png"
            [ Wall, Wall, Path, Wall ] -> Just "tiles/s.png"
            [ Wall, Wall, Wall, Path ] -> Just "tiles/w.png"
            [ Path, Path, Wall, Wall ] -> Just "tiles/ne.png"
            [ Path, Wall, Path, Wall ] -> Just "tiles/ns.png"
            [ Path, Wall, Wall, Path ] -> Just "tiles/nw.png"
            [ Wall, Path, Path, Wall ] -> Just "tiles/es.png"
            [ Wall, Path, Wall, Path ] -> Just "tiles/ew.png"
            [ Wall, Wall, Path, Path ] -> Just "tiles/sw.png"
            [ Path, Path, Path, Wall ] -> Just "tiles/nes.png"
            [ Path, Path, Wall, Path ] -> Just "tiles/new.png"
            [ Path, Wall, Path, Path ] -> Just "tiles/nsw.png"
            [ Wall, Path, Path, Path ] -> Just "tiles/esw.png"
            [ Path, Path, Path, Path ] -> Just "tiles/nesw.png"
            _ -> Nothing
        )

main : Program () Game Msg
main =
    Game.play testBoard
