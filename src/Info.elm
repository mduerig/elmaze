module Info exposing
    ( Info
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
import Html.Attributes as Attr
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal

type Msg
    = CloseModal
    | AnimateModal Modal.Visibility

type alias Content = List ( Html Msg )
type Info =
    Info Modal.Visibility Content Content

init : Bool -> Content -> Content -> Info
init visible =
    Info ( if visible then Modal.shown else Modal.hidden )

show : Info -> Info
show ( Info _ ttl txt ) =
    Info Modal.shown ttl txt

hide : Info -> Info
hide ( Info _ ttl txt ) =
    Info Modal.hidden ttl txt

update : Msg -> Info -> Info
update msg (( Info _ ttl txt ) as info ) =
    case msg of
        CloseModal              -> hide info
        AnimateModal visibility -> Info visibility ttl txt

title : Content -> Modal.Config Msg -> Modal.Config Msg
title = Modal.h3 []

body : Content -> Modal.Config Msg -> Modal.Config Msg
body = Modal.body []

view : Info -> Html Msg
view ( Info visible ttl txt ) =
    Modal.config CloseModal
        |> Modal.withAnimation AnimateModal
        |> Modal.large
        |> Modal.h3 [] ttl
        |> Modal.body [] txt
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.onClick ( AnimateModal Modal.hiddenAnimated )
                , Button.attrs [ Attr.autofocus True ]
                ]
                [ text "Got it!" ]
            ]
        |> Modal.view visible

subscriptions : Info -> Sub Msg
subscriptions ( Info visible _ _ ) =
    Modal.subscriptions visible AnimateModal
