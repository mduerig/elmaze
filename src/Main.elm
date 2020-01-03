module Main exposing ( main )

import Game exposing ( Game, Msg, TileSet, Boundary( .. ), play )
import TestLevel
import Level1
import Level2
import Level3
import Level4
import Level5
import Level6
import Level7
import Level8
import Level9
import Level10
import Level11
import Level12
import Level13

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
    , Level3.level tileSet
    , Level4.level tileSet
    , Level5.level tileSet
    , Level6.level tileSet
    , Level7.level tileSet
    , Level8.level tileSet
    , Level9.level tileSet
    , Level10.level tileSet
    , Level11.level tileSet
    , Level12.level tileSet
    , Level13.level tileSet
    -- , TestLevel.level tileSet
    ]
