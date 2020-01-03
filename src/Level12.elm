module Level12 exposing ( level )

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
    | title = "Full circle for a while"
    , board = board tileSet
    , programText = "let fastForward = while free forward\nlet oneSide = [fastForward, left]\n"
    , infoTitle = [ Html.text "🐞 Full cirlce for a while" ]
    , infoText =
        [ Html.p [] [ Html.text "Help the beetle 🐞 to find the flower 🌺. " ]
        , Html.p [] [ Html.text "Can you write a program that uses 'oneSide' once together with 'while'?" ]
        ]
    }
