module Level1 exposing ( level )

import Html
import Game exposing
    ( TileSet
    , Board
    , Level
    , emptyBoard
    , withTileSet
    , withPath
    , deadEnd
    , withStartAt
    , withGoalAt
    , withActor
    )
import Actor exposing ( Direction ( .. ), hero )

board : TileSet -> Board
board tileSet = emptyBoard 5 5
    |> withTileSet tileSet
    |> withPath ( 0, 0 )
        ( [ Up, Up, Up, Right, Right, Down, Down, Right, Right, Up, Up, Up ]
            |> deadEnd
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 4, 4 )
    |> withActor ( hero 0 ( 0, 0 ) Up "🐞" )

level : TileSet -> Level
level tileSet =
    { title = "Navigate the Maze"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Navigate the Maze"]
    , infoText =
        [ Html.text
            (  "Help the beetle 🐞 to find the flower 🌺. Use the arrow keys to navigate through the maze. "
            ++ "Take care not to bump into walls."
            )
        ]
    }
