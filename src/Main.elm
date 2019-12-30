module Main exposing ( main )

import Game exposing ( .. )
import Actor as A

testBoard : Board
testBoard = emptyBoard 8 6
    |> withTileSet
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
    |> withPath ( 0, 0 )
        ( [A.Up]
            |> fork2
                ( [ A.Up, A.Up, A.Right, A.Right, A.Up, A.Up, A.Right, A.Right, A.Right, A.Right, A.Right, A.Down ]
                    |> deadEnd
                )
                ( [ A.Right, A.Right, A.Right, A.Right ]
                    |>fork2
                        ( [ A.Up, A.Up ]
                            |> fork2
                                ( [A.Left, A.Left]
                                    |> deadEnd
                                )
                                ( [A.Up, A.Right, A.Up]
                                    |> deadEnd
                                )
                        )
                        ( [ A.Down, A.Right, A.Right, A.Up, A.Up, A.Left, A.Left]
                            |> deadEnd
                        )
                )
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 7, 4 )
    |> withActor ( A.hero 0 ( 0, 0 ) A.Up "🐞" )
    |> withActor ( A.friend 1 ( 1, 1 ) A.Up  "🦋" )

main : Program () Game Msg
main = play testBoard
