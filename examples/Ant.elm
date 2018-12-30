module GameOfLife exposing (main)

import Render exposing (render)
import CellAutomata as Automata exposing (rot90Symmetry,anyNeighborhood,RuleExpression(..),Location,Grid,Automata)
import Dict exposing (Dict)
import Html exposing (Html)

type State = Wall
            | Up
            | Down
            | Left
            | Right

init : Grid State
init =
    Dict.empty
        |> Dict.insert (0,1) Up
        |> Dict.insert (1,1) Wall

order maybeState =
  case maybeState of
      Nothing ->
          0

      Just Wall ->
          1

      Just Up ->
          2

      Just Down ->
          3

      Just Left ->
          4

      Just Right ->
          5

automata : Automata State
automata =
    let
        rotState : State -> State
        rotState state =
            case state of
                Up ->
                    Right

                Right ->
                    Down

                Down ->
                    Left

                Left ->
                    Up
                
                a ->
                    a
    in
    [ { from = Just Up
      , neighbors = anyNeighborhood
      , to = Nothing
      }
    , { from = Nothing
      , to = Just Up
      , neighbors =
            { anyNeighborhood
            | south = Exactly <| Just Up
            , southEast = Exactly <| Just Wall
            }
      }
    , { from = Nothing
      , to = Just Right
      , neighbors =
            { anyNeighborhood
            | south = Exactly <| Just Wall
            , west = Exactly <| Just Up }
            }
    ]
        |> Automata.automata (rot90Symmetry rotState) order

main =
  render
    "Game of Life"
    (Automata.step automata)
    (\state ->
        case state of
            Nothing -> Html.text "🔲"
            Just Wall -> Html.text "🔴"
            Just Down -> Html.text "⬇"
            Just Up -> Html.text "⬆"
            Just Left -> Html.text "⬅"
            Just Right -> Html.text "➡"
    )
    init