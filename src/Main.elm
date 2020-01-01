module Main exposing ( main )

import Game exposing ( Game, Msg, TileSet, Boundary( .. ), play )
import TestLevel
import Level1
import Level2

tileSet : TileSet
tileSet north east south west = case [ north, east, south, west ] of
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

main : Program () Game Msg
main = play
    [ Level1.level tileSet
    , Level2.level tileSet
    , TestLevel.level tileSet
    ]
