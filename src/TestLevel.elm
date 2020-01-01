module TestLevel exposing ( level )

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
import Actor exposing ( Direction ( .. ), hero, friend )

board : TileSet -> Board
board tileSet = emptyBoard 8 6
    |> withTileSet tileSet
    |> withPath ( 0, 0 )
        ( [ Up ]
            |> fork2
                ( [ Up, Up, Right, Right, Up, Up, Right, Right, Right, Right, Right, Down ]
                    |> deadEnd
                )
                ( [ Right, Right, Right, Right ]
                    |>fork2
                        ( [ Up, Up ]
                            |> fork2
                                ( [Left, Left]
                                    |> deadEnd
                                )
                                ( [Up, Right, Up]
                                    |> deadEnd
                                )
                        )
                        ( [ Down, Right, Right, Up, Up, Left, Left]
                            |> deadEnd
                        )
                )
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 7, 4 )
    |> withActor ( hero 0 ( 0, 0 ) Up "🐞" )
    |> withActor ( friend 1 ( 1, 1 ) Up  "🦋" )

level : TileSet -> Level
level tileSet =
    { emptyLevel
    | title = "Test Level"
    , board = board tileSet
    , infoTitle = [ Html.text "Test"]
    , infoText = [ Html.text "This is a test level" ]
    }
