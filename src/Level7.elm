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
    | title = "Stop before it's too late"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Stop before it's too late" ]
    , infoText =
        [ Html.p [] [ Html.text "Help the beetle 🐞 to find the flower 🌺. " ]
        , Html.p [] [ Html.text "Use 'while' to move forward until reaching the flower 🌺." ]
        , Html.p [] [ Html.text "The condition 'goal' holds when reaching the flower." ]
        , Html.p [] [ Html.text "Tip: you can use 'not' to negate a condition." ]
        ]
    }
