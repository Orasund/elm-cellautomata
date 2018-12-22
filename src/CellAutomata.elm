module CellAutomata exposing (step,noSymmetry,automata,neighborhoodFunction,ruleSet,Rule,RuleSet,GridAutomata,Neighborhood,Grid,Location)

{-|
## Types
@docs Grid,Location

## Rule
@docs RuleExpression,Neighborhood,anyNeigborhood,Rule

## Basic Automata
@docs step,Automata,automata
-}

import CellAutomata.General exposing (Field,Symmetry,Automata,RuleExpression(..),NeighborhoodFunction,Symmetry)
import Dict exposing (Dict)

{-| The location is the unique identifier for any cell.  
For our purpose we use `(x,y)`-coordinates.  

**Note:** The south of `(0,0)` is `(0,y)` while the north is `(0,-y)`.
-}
type alias Location =
    (Int,Int)

{-| The grid is the *model* of this module.

You might want to write your own view function for it
or else you can't see what the automata has done.

In your head you should think of this as a grid, where some cells are filled in.  
In fact, only the filled cells are stored in the Dict.

Cells have the type `Maybe state` where `state` should be a custom type.
(See CellAutomata.LifeLike for a idea of how the state may be implemented.)
-}
type alias Grid state =
    Field Location state

{-| RuleExpressions give us a very flexible way of talking about neighbors.

Saying something is `Anything`, it means its value is ignored.
-}
type RuleExpression state
    = Exactly state
    | Anything

{-| The Neighborhood of a cell consists of the 8 surounding cells.

If some neighbor may have any value (that is most often the case),  
its best to use the anyNeighborhood template and start from there.  
-}
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

{-| this template helps defining a Neighborhood.

For example, if we would want to only consider the 4 adjacent neighbors,
we might specify it the following way

    {anyNeighborhood
    | north : Exactly a
    , east : Exactly b
    , south : Exactly c
    , west : Exactly d
    }
-}
anyNeigborhood : Neighborhood (RuleExpression (Maybe state))
anyNeigborhood =
    { north = Anything
    , northEast = Anything
    , east = Anything
    , southEast = Anything
    , south = Anything
    , southWest = Anything
    , west = Anything
    , northWest = Anything
    }

{-| A rule consist of the following elements:
* **from** - the state of the cell
* **neighbors** - the nessesary pattern of the neighbors
* **to** - the state, the cell will transition in, if the rule applies
-}
type alias Rule state =
    { from: state
    , neighbors:Neighborhood (RuleExpression state)
    , to: state
    }

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