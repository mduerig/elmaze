module MenuBar exposing
    ( MenuBar
    , LevelItem
    , init
    , withLevelToggle
    , view
    , subscriptions
    )

import Html exposing ( Html, text )
import Html.Events as Events
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar

type MenuBar = MenuBar MenuBarState

type alias MenuBarState =
    { navBar : Navbar.State
    , levelToggle : String
    }

type alias LevelItem msg =
    { text : String
    , onSelect : msg
    }

init : String -> ( MenuBar -> msg ) -> ( MenuBar, Cmd msg )
init levelToggle onStateChange =
    Navbar.initialState ( newMenuBar levelToggle >> onStateChange )
        |> Tuple.mapFirst ( newMenuBar levelToggle )

newMenuBar : String -> Navbar.State -> MenuBar
newMenuBar levelToggle navBar = MenuBar
    { navBar = navBar
    , levelToggle = levelToggle
    }

withLevelToggle : String -> MenuBar -> MenuBar
withLevelToggle toggle ( MenuBar menuBar ) =
    MenuBar { menuBar | levelToggle = toggle }

view : ( MenuBar -> msg ) -> msg -> List ( LevelItem msg ) -> MenuBar -> Html msg
view onStateChange onHelp levelItems ( MenuBar menuBar )
    = Grid.container []
    [ Navbar.config ( withNavBar menuBar >> onStateChange )
        |> Navbar.withAnimation
        |> Navbar.collapseMedium
        |> Navbar.light
        |> Navbar.brand
            [ ]
            [ text "🐞 Elmaze" ]
        |> Navbar.items
            [ Navbar.dropdown
                { id = "levelDropDown"
                , toggle = Navbar.dropdownToggle [] [ text menuBar.levelToggle ]
                , items = levelItems
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
        |> Navbar.view menuBar.navBar
    ]

levelItem : LevelItem msg -> Navbar.DropdownItem msg
levelItem { text, onSelect } =
    Navbar.dropdownItem [ Events.onClick onSelect ] [ Html.text text ]

subscriptions : ( MenuBar -> msg ) -> MenuBar -> Sub msg
subscriptions onStateChange ( MenuBar menuBar ) =
    Navbar.subscriptions menuBar.navBar ( withNavBar menuBar >> onStateChange )

withNavBar : MenuBarState -> Navbar.State -> MenuBar
withNavBar menuBar navBar =
    MenuBar { menuBar | navBar = navBar }

