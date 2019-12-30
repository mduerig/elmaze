module Info exposing
    ( Model
    , Msg
    , show
    , hide
    , init
    , update
    , title
    , body
    , view
    , subscriptions
    )

import Html exposing ( Html, text )
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal

type Msg
    = CloseModal
    | AnimateModal Modal.Visibility

type alias Content = List ( Html Msg )
type Model =
    Model Modal.Visibility Content Content

init : Bool -> Content -> Content -> Model
init visible =
    Model ( if visible then Modal.shown else Modal.hidden )

show : Model -> Model
show ( Model _ ttl txt ) =
    Model Modal.shown ttl txt

hide : Model -> Model
hide ( Model _ ttl txt ) =
    Model Modal.hidden ttl txt

update : Msg -> Model -> Model
update msg (( Model _ ttl txt ) as model ) =
    case msg of
        CloseModal              -> hide model
        AnimateModal visibility -> Model visibility ttl txt

title : Content -> Modal.Config Msg -> Modal.Config Msg
title = Modal.h3 []

body : Content -> Modal.Config Msg -> Modal.Config Msg
body = Modal.body []

view : Model -> Html Msg
view ( Model visible ttl txt ) =
    Modal.config CloseModal
        |> Modal.withAnimation AnimateModal
        |> Modal.large
        |> Modal.h3 [] ttl
        |> Modal.body [] txt
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.onClick ( AnimateModal Modal.hiddenAnimated )
                ]
                [ text "Got it!" ]
            ]
        |> Modal.view visible

subscriptions : Model -> Sub Msg
subscriptions ( Model visible _ _ ) =
    Modal.subscriptions visible AnimateModal
