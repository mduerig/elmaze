module Level2 exposing ( level )

import Html
import Game exposing
    ( TileSet
    , Board
    , Level
    , emptyLevel
    , emptyBoard
    , withTileSet
    , withPath
    , deadEnd
    , withStartAt
    , withGoalAt
    , withActor
    )
import Levels exposing ( text, em, p )
import Actor exposing ( Direction ( .. ), hero )

board : TileSet -> Board
board tileSet = emptyBoard 5 5
    |> withTileSet tileSet
    |> withPath ( 0, 0 )
        ( [ Right, Right, Right, Up, Up, Left, Left, Up, Up, Right, Right, Right ]
            |> deadEnd
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 4, 4 )
    |> withActor ( hero 0 ( 0, 0 ) Right "🐞" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Record a Trace   ⭐"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Record a Trace" ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Observe the trace that is recorded in the text box to the left of the maze. " ]
        , p [ text "Press the ", em "Go! ", text "button to replay the recorded trace and observe the beetle 🐞 when it reaches the flower 🌺." ]
        ]
    }
