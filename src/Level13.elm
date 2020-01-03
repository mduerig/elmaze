module Level13 exposing ( level )

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

rightDown : Int ->  List Direction
rightDown n = List.repeat n Right ++ List.repeat n Down

leftUp : Int ->  List Direction
leftUp n = List.repeat n Left ++ List.repeat n Up

board : TileSet -> Board
board tileSet = emptyBoard 8 8
    |> withTileSet tileSet
    |> withPath ( 0, 0 )
        (  List.repeat 7 Up
        ++ rightDown 7
        ++ leftUp 6
        ++ rightDown 5
        ++ leftUp 4
        ++ rightDown 3
        ++ leftUp 2
        ++ rightDown 1
            |> deadEnd
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 4, 3 )
    |> withActor ( hero 0 ( 0, 0 ) Left "🐞" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Dizzy now   ⭐⭐⭐⭐"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Dizzy now" ]
    , infoText =
        [ Html.p [] [ Html.text "Help the beetle 🐞 to find the flower 🌺. " ]
        , Html.p [] [ Html.text "Write a program using let bindings and 'while' like in the previous levels." ]
        ]
    }
