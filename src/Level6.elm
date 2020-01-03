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
        [ Html.p [] [ Html.text "Help the beetle 🐞 to find the flower 🌺. " ]
        , Html.p [] [ Html.text "Instead of using 'repeat' with a fixed number you can use 'while' with a condition." ]
        , Html.p [] [ Html.text "Try with 'while free forward'. " ]
        ]
    }
