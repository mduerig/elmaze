module Main exposing (..)

import Array exposing (..)

import Browser exposing ( document, Document )
import Collage exposing (..)
import Collage.Text as Text
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Html exposing ( Html )

type alias Flags = { }

type alias Board = Array Cell

type alias Cell =
  { x : Int
  , y : Int
  , cellType : CellType
  , top : Boundary
  , left : Boundary
  , bottom : Boundary
  , right : Boundary
  }

type CellType
  = Empty
  | Start
  | Goal

type Boundary
  = Wall
  | Free

type Player = Player Int Int

type alias Model =
  ( Board
  , Player
  )

type Msg
  = Switch
  | Tick

subscriptions : Model -> Sub msg
subscriptions model = Sub.none

  --   ___
  --  |   |
  --  |xxx|
  --  |___|
  --
initBoard : Board
initBoard =
  let
    cell =
      { x = 0
      , y = 0
      , cellType = Empty
      , top = Free
      , left = Free
      , bottom = Free
      , right = Free
      }
  in
    Array.fromList
      [ { cell | left = Wall, top = Wall },    { cell | top = Wall },    { cell | top = Wall, right = Wall }
      , { cell | left = Wall },                  cell,                   { cell | right = Wall }
      , { cell | left = Wall, bottom = Wall }, { cell | bottom = Wall }, { cell | bottom = Wall, right = Wall }
      ]
    |> Array.indexedMap ( \i -> \c ->
      { c
      | x = modBy 3 i
      , y = 2 - i // 3
      } )


initPlayer : Player
initPlayer = Player 0 0

init : Flags -> ( Model, Cmd msg )
init flags =
  ( ( initBoard, initPlayer )
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model
  , Cmd.none
  )

renderCell : Cell -> Collage Msg
renderCell cell =
  let
      wallStyle wall = case wall of
        Wall -> solid thin ( uniform black )
        Free -> invisible
  in
    group
      [ line 50
          |> traced ( wallStyle cell.bottom )
          |> shiftY -25
      , line 50
          |> traced ( wallStyle cell.top )
          |> shiftY 25
      , line 50
          |> traced ( wallStyle cell.right )
          |> rotate (pi/2)
          |> shiftX 25
      , line 50
          |> traced ( wallStyle cell.left )
          |> rotate (pi/2)
          |> shiftX -25
      , Text.fromString (String.fromInt cell.x ++ "," ++ String.fromInt cell.y)
          |> rendered
      , square 50
          |> filled ( uniform lightYellow )
      ]
      |> shift (50 * toFloat cell.x, 50 * toFloat cell.y)

renderBoard : Board -> List ( Html Msg )
renderBoard board =
  [ board
      |> Array.map renderCell
      |> Array.toList
      |> group
      |> svg
  ]

view : Model -> Document Msg
view ( board, player ) =
  { title = "Testi"
  , body = renderBoard board
  }

main : Program Flags Model Msg
main = document
  { subscriptions = subscriptions
  , init = init
  , update = update
  , view = view
  }