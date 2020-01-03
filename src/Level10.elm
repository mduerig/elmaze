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
    | title = "Full circle reloaded"
    , board = board tileSet
    , programText = "let fastForward = while free forward\n"
    , infoTitle = [ Html.text "🐞 Full cirlce reloaded" ]
    , infoText =
        [ Html.p [] [ Html.text "Help the beetle 🐞 to find the flower 🌺. " ]
        , Html.p [] [ Html.text "Instead of repeating them same 'while' statment you can also define a shortcut for it via a let binding." ]
        , Html.p [] [ Html.text "The let binding 'let fastForward = while free forward' makes 'fastForward' a shortcut for 'while free forward'." ]
        , Html.p [] [ Html.text "Subsequent occurences of 'fastForward' in the program are replaced with 'while free forward'." ]
        , Html.p [] [ Html.text "How many time do you have to use 'fastForward' in you program?" ]
        ]
    }
