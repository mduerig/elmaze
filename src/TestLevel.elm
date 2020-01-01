module TestLevel exposing ( level )

import Html
import Game exposing
    ( TileSet
    , Board
    , Level
    , emptyBoard
    , withTileSet
    , withPath
    , fork2
    , deadEnd
    , withStartAt
    , withGoalAt
    , withActor
    )
import Actor as A

board : TileSet -> Board
board tileSet = emptyBoard 8 6
    |> withTileSet tileSet
    |> withPath ( 0, 0 )
        ( [A.Up]
            |> fork2
                ( [ A.Up, A.Up, A.Right, A.Right, A.Up, A.Up, A.Right, A.Right, A.Right, A.Right, A.Right, A.Down ]
                    |> deadEnd
                )
                ( [ A.Right, A.Right, A.Right, A.Right ]
                    |>fork2
                        ( [ A.Up, A.Up ]
                            |> fork2
                                ( [A.Left, A.Left]
                                    |> deadEnd
                                )
                                ( [A.Up, A.Right, A.Up]
                                    |> deadEnd
                                )
                        )
                        ( [ A.Down, A.Right, A.Right, A.Up, A.Up, A.Left, A.Left]
                            |> deadEnd
                        )
                )
        )
    |> withStartAt ( 0, 0 )
    |> withGoalAt ( 7, 4 )
    |> withActor ( A.hero 0 ( 0, 0 ) A.Up "🐞" )
    |> withActor ( A.friend 1 ( 1, 1 ) A.Up  "🦋" )

level : TileSet -> Level
level tileSet =
    { title = "Test Level"
    , board = board tileSet
    , infoTitle = [ Html.text "Test"]
    , infoText = [ Html.text "This is a test level" ]
    }
