module Chapter.GameOfLife exposing (..)

import CellAutomata.New as CellAutomata
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
                CellAutomata.new
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
Conway's Game of Life is the most known cell automata.

The Rules are simple:

* A "Dead" cell becomes "Alive" if there are exactly 3 alive neighbors.
* An "Alive" cell stays alive if it has 2 or 3 neighbors.

<component with-label="Editor"/>

We can turn these rules directly into code:

```
CellAutomata.new
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
