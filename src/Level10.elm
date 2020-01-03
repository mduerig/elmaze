module Level10 exposing ( level )

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
    | title = "Full circle reloaded   ⭐⭐⭐"
    , board = board tileSet
    , programText = "let fastForward = while free forward\n"
    , infoTitle = [ Html.text "🐞 Full cirlce reloaded" ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Instead of repeating them same ", em "while", text " statment you can also define a shortcut for it via a let binding." ]
        , p [ text "The let binding ", em "let fastForward = while free forward", text " makes ", em "fastForward", text " a shortcut for ", em "while free forward", text "." ]
        , p [ text "Subsequent occurences of ", em "fastForward", text " in the program are replaced with ", em "while free forward", text "." ]
        , p [ text "How many time do you have to use ", em "fastForward", text " in you program?" ]
        ]
    }
