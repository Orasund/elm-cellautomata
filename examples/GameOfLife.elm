module GameOfLife exposing (main)

import Render exposing (render)
import CellAutomata.LifeLike as Automata exposing (Location,Grid,AliveNeighbors(..),Automata,State(..))
import Dict exposing (Dict)
import Html exposing (Html)

init : Grid
init =
    Dict.empty
        |> Dict.insert (0,1) Alive
        |> Dict.insert (1,2) Alive
        |> Dict.insert (2,0) Alive
        |> Dict.insert (2,1) Alive
        |> Dict.insert (2,2) Alive

automata : Automata
automata =
    [ {from = Just Alive, neighbors = TwoAlive, to = Just Alive}
    , {from = Just Alive, neighbors = ThreeAlive, to = Just Alive}
    , {from = Just Alive, neighbors = AnyAmount, to = Nothing}
    , {from = Nothing, neighbors = ThreeAlive, to = Just Alive}
    ]
    |>Automata.automata

main =
  render
    "Game of Life"
    (Automata.step automata)
    (\state ->
        case state of
            Nothing -> Html.text "🔲"
            Just Alive -> Html.text "🔴"
    )
    init