module CellAutomata.Grid2DBased exposing (step,noSymmetry,automata,neighborhoodFunction,ruleSet,Rule,RuleSet,GridAutomata,Neighborhood,Grid,Location)

import CellAutomata exposing (Field,Symmetry,Automata,RuleExpression(..),NeighborhoodFunction,Symmetry)
import Dict exposing (Dict)


type alias Location =
    (Int,Int)


type alias Grid state =
    Field Location state


type alias Neighborhood state =
    { north : state
    , northEast : state
    , east : state
    , southEast : state
    , south : state
    , southWest : state
    , west : state
    , northWest : state
    }

type alias Rule state =
    CellAutomata.Rule (Neighborhood (RuleExpression state)) state 

type alias RuleSet state = 
    CellAutomata.RuleSet (Neighborhood (RuleExpression state)) state

ruleSet : Dict Int (List (Rule state)) -> RuleSet state
ruleSet = CellAutomata.RuleSet

type alias GridAutomata state =
    Automata (Neighborhood (Maybe state)) (Neighborhood (RuleExpression (Maybe state))) Location state

automata : { rules:Dict Int (List (Rule (Maybe state)))
  , symmetry : Symmetry (Neighborhood (Maybe state)) (Neighborhood (RuleExpression (Maybe state))) (Maybe state)
  , order: (Maybe state) -> Int
  } -> GridAutomata state
automata {rules,symmetry,order}=
    { ruleSet=ruleSet rules
  , symmetry = symmetry
  , neighborhoodFunction = neighborhoodFunction
  , order= order
  }


neighborhoodFunction : NeighborhoodFunction Location (Neighborhood (Maybe state)) state
neighborhoodFunction ((x,y) as location) field =
    { north = field |> Dict.get (x, y - 1)
    , northEast = field |> Dict.get (x + 1, y - 1)
    , east = field |> Dict.get (x + 1,y)
    , southEast = field |> Dict.get (x + 1,y + 1)
    , south = field |> Dict.get (x,y + 1)
    , southWest = field |> Dict.get (x - 1, y + 1)
    , west = field |> Dict.get (x - 1,y)
    , northWest = field |> Dict.get (x - 1,y - 1)
    }


noSymmetry : Symmetry (Neighborhood state) (Neighborhood (RuleExpression state)) state
noSymmetry state neighborhood {neighbors,from} =
        (state == from)
        && (neighbors.north
                == Anything
                || Exactly neighborhood.north
                == neighbors.north
        )
        && (neighbors.northEast
                == Anything
                || Exactly neighborhood.northEast
                == neighbors.northEast
           )
        && (neighbors.east
                == Anything
                || Exactly neighborhood.east
                == neighbors.east
           )
        && (neighbors.southEast
                == Anything
                || Exactly neighborhood.southEast
                == neighbors.southEast
           )
        && (neighbors.south
                == Anything
                || Exactly neighborhood.south
                == neighbors.south
           )
        && (neighbors.southWest
                == Anything
                || Exactly neighborhood.southWest
                == neighbors.southWest
           )
        && (neighbors.west
                == Anything
                || Exactly neighborhood.west
                == neighbors.west
           )
        && (neighbors.northWest
                == Anything
                || Exactly neighborhood.northWest
                == neighbors.northWest
           )

step : GridAutomata state -> Grid state -> (Location-> (Maybe state) -> Maybe state)
step = CellAutomata.step