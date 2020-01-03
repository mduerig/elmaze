module Level8 exposing ( level )

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
board tileSet = emptyBoard 14 14
    |> withTileSet tileSet
    |> withPath ( 0, 0 )
        ( List.repeat 13 Right ++ List.repeat 13 Up
            |> deadEnd
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 13, 13 )
    |> withActor ( hero 0 ( 0, 0 ) Right "🐞" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Forward, forward, forward...   ⭐⭐"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Forward, forward, forward..." ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Use ", em "while", text " to move forward as long as the path is free." ]
        , p [ text "Tip: you have to use ", em "while", text " twice." ]
        ]
    }
