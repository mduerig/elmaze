module Level5 exposing ( level )

import Html
import Game exposing
    ( TileSet
    , Board
    , Level
    , Path
    , emptyLevel
    , emptyBoard
    , withTileSet
    , withPath
    , fork2
    , deadEnd
    , withStartAt
    , withGoalAt
    , withActor
    )
import Levels exposing ( text, em, p )
import Actor exposing ( Direction ( .. ), hero )

square : Path
square = []
    |> fork2
        ( [ Right, Right, Up, Up ] |> deadEnd )
        ( [ Up, Up, Right, Right ] |> deadEnd )

board : TileSet -> Board
board tileSet = emptyBoard 5 5
    |> withTileSet tileSet
    |> withPath ( 0, 0 ) square
    |> withPath ( 2, 2 ) square
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 4, 4 )
    |> withActor ( hero 0 ( 0, 0 ) Right "🐞" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Five lines   ⭐⭐"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Five lines" ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Write a program with no more than five lines. " ]
        , p [ text "Tip: use the ", em "repeat ", text "statement wisely. " ]
        ]
    }
