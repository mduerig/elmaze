module Level8 exposing ( level )

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

right : Int ->  List Direction
right n = List.repeat n Right

left : Int ->  List Direction
left n = List.repeat n Left

up : Int ->  List Direction
up n = List.repeat n Up

down : Int ->  List Direction
down n = List.repeat n Down

board : TileSet -> Board
board tileSet = emptyBoard 5 5
    |> withTileSet tileSet
    |> withPath ( 0, 0 )
        ( right 4 ++ up 4 ++ left 4 ++ down 3 ++ right 3 ++ up 2 ++ left 2 ++ down 1 ++ right 1
            |> deadEnd
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 2, 2 )
    |> withActor ( hero 0 ( 0, 0 ) Right "🐞" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Going in circles"
    , board = board tileSet
    , infoTitle = [ Html.text "🐞 Going in circles" ]
    , infoText =
        [ Html.p [] [ Html.text "Help the beetle 🐞 to find the flower 🌺. " ]
        , Html.p [] [ Html.text "Write a program with no more than five lines. " ]
        , Html.p [] [ Html.text "Tip: use the 'repeat' statement. " ]
        ]
    }
