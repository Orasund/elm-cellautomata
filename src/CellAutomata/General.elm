module CellAutomata.General exposing (Automata(..), Field, NeighborhoodFunction, Rule, RuleExpression(..), RuleSet(..), Symmetry, ruleSet, step)

import Dict exposing (Dict)


type alias Comparable comparable =
    comparable


type alias Field location state =
    Dict (Comparable location) state


type RuleSet neighborhood state comparable
    = RuleSet (Dict comparable (List (Rule neighborhood state)))


ruleSet : (Maybe state -> comparable) -> List (Rule neighborhood (Maybe state)) -> RuleSet neighborhood (Maybe state) comparable
ruleSet order =
    List.foldr
        (\({ from } as r) dict ->
            dict
                |> Dict.update
                    (from |> order)
                    (\maybeList ->
                        case maybeList of
                            Nothing ->
                                Just [ r ]

                            Just list ->
                                Just (r :: list)
                    )
        )
        Dict.empty
        >> RuleSet


type RuleExpression state
    = Exactly state
    | Anything


type alias Rule neighborhood state =
    { from : state
    , neighbors : neighborhood
    , to : state
    }


type alias Symmetry neighborhood ruleNeighborhood state =
    state -> neighborhood -> Rule ruleNeighborhood state -> Maybe state


type alias NeighborhoodFunction location neighborhood state =
    Comparable location -> Field (Comparable location) state -> neighborhood


type Automata neighborhood ruleNeighborhood location state comparable
    = Automata
        { ruleSet : RuleSet ruleNeighborhood (Maybe state) comparable
        , symmetry : Symmetry neighborhood ruleNeighborhood (Maybe state)
        , neighborhoodFunction : NeighborhoodFunction location neighborhood state
        , order : Maybe state -> comparable
        }


step : Automata neighborhood ruleNeighborhood location state comparable -> Field location state -> (location -> Maybe state -> Maybe state)
step (Automata ({ neighborhoodFunction, symmetry, order } as automata)) field =
    \location state ->
        let
            neighborhood : neighborhood
            neighborhood =
                field |> neighborhoodFunction location

            (RuleSet rSet) =
                automata.ruleSet
        in
        rSet
            |> Dict.get (order state)
            |> Maybe.withDefault []
            |> List.filterMap (symmetry state neighborhood)
            |> (\list ->
                    case list of
                        a :: _ ->
                            a

                        _ ->
                            state
               )
