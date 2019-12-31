module MenuBar exposing ( .. )

import Html exposing ( Html, text )
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar

type MenuBar =
    MenuBar Navbar.State

init : ( MenuBar -> msg ) -> ( MenuBar, Cmd msg )
init onStateChange =
    Navbar.initialState ( MenuBar >> onStateChange )
        |> Tuple.mapFirst MenuBar

view : ( MenuBar -> msg ) -> msg -> MenuBar -> Html msg
view onStateChange onHelp ( MenuBar menuBar )
    = Grid.container []
    [ Navbar.config ( MenuBar >> onStateChange )
        |> Navbar.withAnimation
        |> Navbar.collapseMedium
        |> Navbar.light
        |> Navbar.brand
            [ ]
            [ text "🐞 Elmaze" ]
        |> Navbar.items
            [ Navbar.dropdown
                { id = "levelDropDown"
                , toggle = Navbar.dropdownToggle [] [ text "Select a level" ]
                , items =
                    [ Navbar.dropdownItem
                        [ ] [ text "Level 1" ]
                    , Navbar.dropdownItem
                        [ ] [ text "Level 2" ]
                    , Navbar.dropdownItem
                        [ ] [ text "Level 3" ]
                    ]
                }
            ]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ Button.button
                    [ Button.outlineInfo
                    , Button.onClick onHelp
                    ]
                    [ text "Help"]
                ]
            ]
        |> Navbar.view menuBar
    ]

subscriptions : ( MenuBar -> msg ) -> MenuBar -> Sub msg
subscriptions onStateChange ( MenuBar menuBar ) =
    Navbar.subscriptions menuBar ( MenuBar >> onStateChange )
