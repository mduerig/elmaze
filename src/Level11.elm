module Level11 exposing ( level )

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
    | title = "Full circle all the way   ⭐⭐⭐"
    , board = board tileSet
    , programText = "let fastForward = while free forward\nlet oneSide = [fastForward, left]\n"
    , infoTitle = [ Html.text "🐞 Full cirlce all the way" ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Let bindings can also be used in subsequent let bindings." ]
        , p [ text "Here ", em "oneSide = [fastForward, left]", text " causes the beetle turn left after executing ", em "fastForward", text "." ]
        , p [ text "How many time do you use ", em "oneSide", text " in your program?" ]
        , p [ text "Can you write a program that uses ", em "oneSide", text " only once?" ]
        ]
    }
