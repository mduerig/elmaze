module Level9 exposing ( level )

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
board tileSet = emptyBoard 10 10
    |> withTileSet tileSet
    |> withPath ( 0, 0 )
        (  List.repeat 9 Right
        ++ List.repeat 9 Up
        ++ List.repeat 9 Left
        ++ List.repeat 8 Down
            |> deadEnd
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 0, 1 )
    |> withActor ( hero 0 ( 0, 0 ) Right "🐞" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Full circle   ⭐⭐"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Full cirlce" ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Use ", em "while", text " as necessary for long straight streches." ]
        ]
    }
