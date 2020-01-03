module Levels exposing ( text, em, p )

import Html exposing ( Html )

text : String -> Html msg
text s = Html.text s

em : String -> Html msg
em s = Html.em [] [ text s ]

p : List ( Html msg ) -> Html msg
p nodes = Html.p [] nodes