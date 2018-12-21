module CellAutomata exposing (Rule,step,ruleSet,Field,RuleSet(..),RuleExpression(..),Automata(..),Symmetry, NeighborhoodFunction)

import Dict exposing (Dict)


type alias Comparable comparable =
    comparable


type alias Field location state =
    Dict (Comparable location) state


type RuleSet neighborhood state
    = RuleSet (Dict Int (List (Rule neighborhood state)))


ruleSet : ((Maybe state) -> Int) -> List (Rule neighborhood (Maybe state)) -> RuleSet neighborhood (Maybe state)
ruleSet order = List.foldr
    (\({neighbors,from,to} as r) dict ->
        dict
        |> Dict.update
            (from |> order)
            (\maybeList ->
                case maybeList of
                    Nothing -> Just [r]
                    Just list -> Just (r :: list)
            )
    )
    Dict.empty
    >> RuleSet

type RuleExpression state
    = Exactly state
    | Anything

type alias Rule neighborhood state
    = {from:state,neighbors:neighborhood,to:state}

type alias Symmetry neighborhood ruleNeighborhood state
    = state -> neighborhood -> Rule ruleNeighborhood state -> Bool

type alias NeighborhoodFunction location neighborhood state
    = Comparable location -> Field (Comparable location) state -> neighborhood

type Automata neighborhood ruleNeighborhood location state
  = Automata { ruleSet: RuleSet ruleNeighborhood (Maybe state)
  , symmetry : Symmetry neighborhood ruleNeighborhood (Maybe state)
  , neighborhoodFunction : NeighborhoodFunction location neighborhood state
  , order: (Maybe state) -> Int
  }

find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


step : Automata neighborhood ruleNeighborhood location state -> Field location state -> (location-> (Maybe state) -> Maybe state)
step (Automata ({neighborhoodFunction,symmetry,order} as automata)) field=
    \location state ->
        let
            neighborhood : neighborhood
            neighborhood =
                field |> neighborhoodFunction location
            
            (RuleSet rSet) = automata.ruleSet
        in
        rSet
            |> Dict.get (order state)
            |> Maybe.withDefault []
            |> find (symmetry state neighborhood)
            |> Maybe.map .to
            |> Maybe.withDefault state
    