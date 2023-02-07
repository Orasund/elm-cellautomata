module Chapter.Editor exposing (..)

import CellAutomata.Advanced as CellAutomata exposing (Automata)
import CellAutomata.Rule
import Data.Editor exposing (Editor)
import Dict
import ElmBook exposing (Msg)
import ElmBook.Actions
import ElmBook.Chapter exposing (Chapter)
import Html exposing (Html, s)
import Layout
import View.Editor


type alias State =
    { editor : Editor ()
    , automata : Automata ( Int, Int ) ()
    }


init : State
init =
    { editor =
        Data.Editor.init
            { width = 10, height = 10 }
            Dict.empty
    , automata =
        CellAutomata.automata
            { rules =
                [ CellAutomata.Rule.fromNothing
                    |> CellAutomata.Rule.withNeighborhood
                        (\dict ->
                            Dict.size dict == 3
                        )
                    |> CellAutomata.Rule.to ()
                , CellAutomata.Rule.from ()
                    |> CellAutomata.Rule.withNeighborhood
                        (\dict ->
                            Dict.size dict
                                |> (\int -> int < 2 || int > 3)
                        )
                ]
            , neighbors =
                \( x, y ) ->
                    [ ( x + 1, y )
                    , ( x - 1, y )
                    , ( x, y + 1 )
                    , ( x, y - 1 )
                    , ( x + 1, y + 1 )
                    , ( x + 1, y - 1 )
                    , ( x - 1, y + 1 )
                    , ( x - 1, y - 1 )
                    ]
            }
    }


toString : () -> String
toString () =
    "⚫️"


editor : State -> Html (Msg State)
editor state =
    [ View.Editor.display
        { offset = ( 0, 0 )
        , width = state.editor.width
        , height = state.editor.height
        , onClick =
            \( x, y ) ->
                ElmBook.Actions.updateState
                    (\s ->
                        { s
                            | editor =
                                s.editor
                                    |> (\e ->
                                            { e
                                                | dict =
                                                    e.dict
                                                        |> Dict.update ( x, y )
                                                            (\maybe ->
                                                                case maybe of
                                                                    Just () ->
                                                                        Nothing

                                                                    Nothing ->
                                                                        Just ()
                                                            )
                                            }
                                       )
                        }
                    )
                    |> Just
        }
        (state.editor.dict
            |> Dict.map (\_ -> toString)
        )
    , Html.text "Run"
        |> Layout.buttonEl
            { onPress =
                ElmBook.Actions.updateState (\s -> { s | editor = s.editor |> Data.Editor.toggleRunning })
                    |> Just
            , label = "Run"
            }
            []
    ]
        |> Layout.column []


chapter : Chapter State
chapter =
    ElmBook.Chapter.chapter "Editor"
        |> ElmBook.Chapter.withStatefulComponentList
            [ ( "Test", editor ) ]
        |> ElmBook.Chapter.render """Test
        
<component with-label="Test"/>
        """
