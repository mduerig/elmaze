module Main exposing (main)

import Browser exposing ( document )

type alias Flags = { }

type alias Model = { x: Float }

type Msg
  = Switch
  | Tick

subscriptions : Model -> Sub msg
subscriptions model = Sub.none

init : Flags -> ( Model, Cmd msg )
init flags =
  ( { x = 1.0 }
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model
  , Cmd.none
  )

view : Model -> Browser.Document Msg
view model =
  { title = "Testi"
  , body = []
  }

main : Program Flags Model Msg
main = Browser.document
  { subscriptions = subscriptions
  , init = init
  , update = update
  , view = view
  }