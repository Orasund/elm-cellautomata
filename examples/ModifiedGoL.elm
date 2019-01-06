module ModifiedGoL exposing (main)

import CellAutomata.LifeLike as Automata exposing (AliveNeighbors(..), Automata, Grid, Location, RuleExpression(..), State(..), anyNeighborhood)
import Dict exposing (Dict)
import Html exposing (Html)
import Render exposing (render)


init : Grid
init =
    Dict.empty
        |> Dict.insert ( 0, 0 ) Alive


automata : Automata
automata =
    let
        neighbors a b c d =
            { anyNeighborhood
                | north = Exactly a
                , east = Exactly b
                , south = Exactly c
                , west = Exactly d
            }
    in
    [ { from = Just Alive
      , to = Just Alive
      , neighbors = neighbors (Just Alive) Nothing Nothing Nothing
      }
    , { from = Just Alive, to = Nothing, neighbors = anyNeighborhood }
    , { from = Nothing
      , to = Just Alive
      , neighbors = neighbors (Just Alive) Nothing Nothing Nothing
      }
    ]
        |> Automata.automataWithCustomSymmetry Automata.rot90Symmetry


main =
    render
        "Game of Life"
        (Automata.step automata)
        (\state ->
            case state of
                Nothing ->
                    Html.text "🔲"

                Just Alive ->
                    Html.text "🔴"
        )
        init
