module Terrain exposing (main)

import Render exposing (render)
import CellAutomata.LifeLike as Automata exposing (anyNeighborhood,RuleExpression(..),Location,Grid,AliveNeighbors(..),Automata,State(..))
import Dict exposing (Dict)
import Html exposing (Html)

init : Grid
init =
    List.range 0 20
    |> List.foldl
        (\i ->
            Dict.insert (i,0) Alive
        )
        Dict.empty
    |> Dict.insert (4,1) Alive
    |> Dict.insert (12,1) Alive

automata : Automata
automata =
    [ { from = Nothing
      , to = Nothing
      , neighbors =
          { anyNeighborhood 
          | north = Exactly <| Just Alive
          , northEast = Exactly <| Just Alive
          , northWest = Exactly <| Just Alive
          , west = Exactly <| Just Alive
          }
      }
    , { from = Nothing
      , to = Just Alive
      , neighbors =
          { anyNeighborhood 
          | north = Exactly <| Just Alive
          , northEast = Exactly <| Just Alive
          , northWest = Exactly <| Just Alive
          }
      }
    ]
    |> Automata.automataWithCustomSymmetry Automata.vertMirrorSymmetry

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