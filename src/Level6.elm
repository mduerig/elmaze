module Level6 exposing ( level )

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
board tileSet = emptyBoard 15 3
    |> withTileSet tileSet
    |> withPath ( 0, 1 )
        ( List.repeat 14 Right ++ [ Up ]
            |> deadEnd
        )
    |> withStartAt ( 0, 1 )
    |> withGoalAt ( 14, 2 )
    |> withActor ( hero 0 ( 0, 1 ) Right "🐞" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Too lazy to count   ⭐⭐⭐"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Too lazy to count" ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Instead of using ", em "repeat ", text "with a fixed number you can use ", em "while ", text "with a condition." ]
        , p [ text "Try with ", em "while free forward. " ]
        ]
    }
