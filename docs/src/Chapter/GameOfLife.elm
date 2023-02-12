module Chapter.GameOfLife exposing (..)

import CellAutomata.Advanced as CellAutomata
import CellAutomata.Grid
import CellAutomata.Rule
import Data.Editor exposing (Editor)
import Dict
import ElmBook.Chapter exposing (Chapter)
import View.Editor


type alias State =
    ()


init : Editor State
init =
    Dict.empty
        |> Data.Editor.init
            { width = 10
            , height = 10
            , selected = ()
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
                            |> CellAutomata.Rule.toNothing
                        ]
                    , neighbors = CellAutomata.Grid.surroundingNeighbors
                    }
            }


toString : () -> String
toString () =
    "⚫️"


chapter : { read : state -> Editor State, writeTo : state -> Editor State -> state } -> Chapter state
chapter args =
    ElmBook.Chapter.chapter "Conway's Game of Life"
        |> ElmBook.Chapter.withStatefulComponentList
            [ ( "Editor"
              , View.Editor.toHtml
                    { read = args.read
                    , writeTo = args.writeTo
                    , toString = toString
                    , placableStates = [ () ]
                    }
              )
            ]
        |> ElmBook.Chapter.render """
        
<component with-label="Editor"/>

```
CellAutomata.automata
    { rules =
        [ CellAutomata.Rule.fromNothing
            |> CellAutomata.Rule.withNeighborhood
                (\\dict ->
                    Dict.size dict == 3
                )
            |> CellAutomata.Rule.to ()
        , CellAutomata.Rule.from ()
            |> CellAutomata.Rule.withNeighborhood
                (\\dict ->
                    Dict.size dict
                        |> (\\int -> int < 2 || int > 3)
                )
            |> CellAutomata.Rule.toNothing
        ]
    , neighbors = CellAutomata.Grid.surroundingNeighbors
    }
```"""
