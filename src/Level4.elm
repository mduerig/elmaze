module Level4 exposing ( level )

import Html
import Game exposing
    ( TileSet
    , Board
    , Level
    , emptyLevel
    , emptyBoard
    , withTileSet
    , withPath
    , fork2
    , deadEnd
    , withStartAt
    , withGoalAt
    , withActor
    )
import Levels exposing ( text, em, p )
import Actor exposing ( Direction ( .. ), hero )

board : TileSet -> Board
board tileSet = emptyBoard 5 5
    |> withTileSet tileSet
    |> withPath ( 0, 0 )
        ( [ Right, Right, Right, Up, Up ]
            |> fork2
                ( [ Left, Left, Up, Up, Right, Right, Right ]
                    |> deadEnd
                )
                ( [ Up, Up ]
                    |> deadEnd
                )
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 4, 4 )
    |> withActor ( hero 0 ( 0, 0 ) Right "🐞" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Find a shorter program   ⭐⭐"
    , board = board tileSet
    , programText = "forward\nforward\nforward\nleft\nforward\nforward\nforward\nforward\nright\nforward\n"
    , infoTitle = [ Html.text "🐞 Find a shorter program" ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Can you find a shorter program than the one given? " ]
        , p [ text "You can replace repeated commands with a ", em "repeat ", text "statement. " ]
        , p [ text "Try for example ", em "repeat 4 forward", text "." ]
        ]
    }
