module Level3 exposing ( level )

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
    | title = "Find the shortest way   ⭐"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Find the shortest path" ]
    , infoText =
        [ p [ text "Help the beetle 🐞 to find the flower 🌺. " ]
        , p [ text "Can you find the shortest path? " ]
        , p [ text "Observe the traces for different paths and replay them using the ", em "Go! ", text "button." ]
        ]
    }
