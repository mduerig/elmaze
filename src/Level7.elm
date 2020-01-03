module Level7 exposing ( level )

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
        ( List.repeat 14 Right
            |> deadEnd
        )
    |> withStartAt ( 0, 1 )
    |> withGoalAt ( 12, 1 )
    |> withActor ( hero 0 ( 0, 1 ) Right "🐞" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Stop before it's too late   ⭐⭐⭐"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Stop before it's too late" ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Use ", em "while", text " to move forward until reaching the flower 🌺." ]
        , p [ text "The condition ", em "goal", text " holds when reaching the flower." ]
        , p [ text "Tip: you can use ", em "not", text " to negate a condition." ]
        ]
    }
