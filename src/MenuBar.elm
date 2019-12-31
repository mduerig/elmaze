module MenuBar exposing ( .. )

import Html exposing ( Html, text )
import Html.Events as Events
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar

type MenuBar =
    MenuBar Navbar.State

type alias LevelItem msg =
    { text : List ( Html msg )
    , onSelect : msg
    }

init : ( MenuBar -> msg ) -> ( MenuBar, Cmd msg )
init onStateChange =
    Navbar.initialState ( MenuBar >> onStateChange )
        |> Tuple.mapFirst MenuBar

view : ( MenuBar -> msg ) -> msg -> List ( LevelItem msg ) -> MenuBar -> Html msg
view onStateChange onHelp items ( MenuBar menuBar )
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
                , items = items
                    |> List.map levelItem
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

levelItem : LevelItem msg -> Navbar.DropdownItem msg
levelItem { text, onSelect } =
    Navbar.dropdownItem [ Events.onClick onSelect ] text

subscriptions : ( MenuBar -> msg ) -> MenuBar -> Sub msg
subscriptions onStateChange ( MenuBar menuBar ) =
    Navbar.subscriptions menuBar ( MenuBar >> onStateChange )
