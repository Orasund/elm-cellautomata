module CellAutomata exposing (step,noSymmetry,automata,neighborhoodFunction,ruleSet,Rule,RuleSet,GridAutomata,Neighborhood,Grid,Location)

{-| If you are new to this package, consider checking out CellAutomata.LifeLike first,
as it is written as an introduction to this module.

**If you want to create cellAutomatas with more then one state,**  
**but still operates on a 2D-Grid, than this package is the right one for you.**

First start by writing your own state type.
As an example, lets try to simulate a ant that trys to escape a mase.
* The ant will try to always follow right wall.

Our state will now be the following

    type State =
        Wall,
        Left,
        Up,
        Right,
        Down
# The Basics
## Types
@docs Order,Grid,Location

## Rule
@docs RuleExpression,Neighborhood,anyNeigborhood,Rule

## Automata Without Symmetry
@docs step,Automata,automataWithNoSymmetry

# Symmetries
Now that we need to specify our own state, it is no longer possible to write a
fullSymmetry-Function(and it is not adviced to do so).

## Symmetry
@docs Symmetry,noSymmetry,horMirrorSymmetry,vertMirrorSymmetry,rot45Symmetry,rot90Symmetry

## Automata
@docs automata
-}

import CellAutomata.General as General
import Dict exposing (Dict)

{-| Every State needs a defined order.
(e.g. a function that gives each state a unique identifer)

For example the order function for Langton's ant is the following.

    order maybeState =
        case maybeState of
            Nothing -> 0
            Just Wall -> 1
            Just Up -> 2
            Just Down -> 3
            Just Left -> 4
            Just Right -> 5
-}
type alias Order state =
    Maybe state -> Int

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

For example, if we would want to only consider the north neighbor,
we might specify it the following way

    {anyNeighborhood
    | north : Exactly a
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
    { from: Maybe state
    , neighbors:Neighborhood (RuleExpression (Maybe state) )
    , to: Maybe state
    }

type alias RuleSet state = 
    CellAutomata.RuleSet (Neighborhood (RuleExpression state)) state

ruleSet : Dict Int (List (Rule state)) -> RuleSet state
ruleSet = CellAutomata.RuleSet

{-| The Automata type can be seen as a config type.  
Its stores all information to specify the behaviour of a cell automata.  
Sometimes more then one automata should act on to the same Grid.  
For this reason it is its own type.
-}
type alias Automata state =
    General.Automata
        (Neighborhood (Maybe state))
        (Neighborhood (RuleExpression (Maybe state)))
        Location
        state

type alias Symmetry state
    = (Maybe state) -> Neighborhood (Maybe state) -> Rule state -> Bool

{-| This function uses no symmetry, this means every possible combination must be
specified.

    automataWithoutSymmetry = automataWithCustomSymmetry noSymmetry

**This function is not useful in practice.**  
**Most often you want at least rotational or mirrored symmetry**  
**This function is only included for demonstration purposes**

Checkout CellAutomata.LifeLike for a more detailed discribtion.

For example the rules for Langton's ant are the following:
* if a white cell is in front of
-}
automataWithoutSymmetry : Order -> List (Rule state) -> Automata state
automataWithoutSymmetry order listOfRules=
    { ruleSet = listOfRules
        |> General.ruleSet order
    , symmetry = noSymmetry
    , neighborhoodFunction = neighborhoodFunction
    , order= order
    }

automata : Symmertry state -> Order -> List (Rule state) -> Automata state
automata symmetry order listOfRules=
    { ruleSet = listOfRules
        |> General.ruleSet order
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


subFuncCompareLists :
    Neighborhood (RuleExpression (Maybe state))
    -> Neighborhood (Maybe state)
    -> List (Neighborhood (RuleExpression (Maybe state)) -> (RuleExpression (Maybe state))) 
    -> List (Neighborhood (Maybe state) -> (Maybe state))
    -> Bool
subFuncCompareLists neighbors neighborhood directions dirList =
    let
        compare : (RuleExpression (Maybe State)) -> (Maybe State) -> Bool
        compare expression b =
            case expression of
                Exactly a -> a == b
                Anything -> True
    in
    List.map2
        (\dir1 dir2 -> compare (dir1 neighbors) (dir2 neighborhood))
        directions
        dirList
    |> List.all identity

subFuncRotate : List (Neighborhood a -> a) -> List (Neighborhood a -> a)
subFuncRotate list = case list of
    [] -> []
    a :: tail -> List.append tail [a]

{-| Pattern may be horizontally mirrored
-}
horMirrorSymmetry : Symmetry state
horMirrorSymmetry state neighborhood {from,neighbors} =
    let
        directions : List (Neighborhood a -> a)
        directions = [.north,.northEast,.east,.southEast,.south,.southWest,.west,.northWest]

        mirrored : List (Neighborhood a -> a)
        mirrored = [.south,.southEast,.east,.northEast,.north,.northWest,.west,.southWest]

        compareLists = 
            subFuncCompareLists neighbors neighborhood directions
    in
    (state == from)
    &&
    (
        (compareLists directions)
        || (compareList mirrored)
    )

{-| Pattern may be vertically mirrored
-}
vertMirrorSymmetry : Symmetry state
vertMirrorSymmetry state neighborhood {from,neighbors} =
    let
        directions : List (Neighborhood a -> a)
        directions = [.north,.northEast,.east,.southEast,.south,.southWest,.west,.northWest]

        mirrored : List (Neighborhood a -> a)
        mirrored = [.north,.northWest,.west,.southWest,.south,.southEast,.east,.northEast]

        compareLists = 
            subFuncCompareLists neighbors neighborhood directions
    in
    (state == from)
    &&
    (
        (compareLists directions)
        || (compareList mirrored)
    )

{-| Pattern may be rotated in any position.
-}
rot45Symmetry : Symmetry state
rot45Symmetry state neighborhood {from,neighbors} =
    let
        directions : List (Neighborhood a -> a)
        directions = [.north,.northEast,.east,.southEast,.south,.southWest,.west,.northWest]
        
        rot : List (Neighborhood a -> a) -> List (Neighborhood a -> a)
        rot = subFuncRotate

        rot1 : List (Neighborhood a -> a)
        rot1 = directions |> rot

        rot2 : List (Neighborhood a -> a)
        rot2 = rot1 |> rot

        rot3 : List (Neighborhood a -> a)
        rot3 = rot2 |> rot

        rot4 : List (Neighborhood a -> a)
        rot4 = rot3 |> rot

        rot5 : List (Neighborhood a -> a)
        rot5 = rot4 |> rot

        rot6 : List (Neighborhood a -> a)
        rot6 = rot5 |> rot

        rot7 : List (Neighborhood a -> a)
        rot7 = rot6 |> rot

        compareLists = 
            subFuncCompareLists neighbors neighborhood directions
    in
    (state == from)
    && 
    (   (compareLists directions)
        || (compareLists rot1)
        || (compareLists rot2)
        || (compareLists rot3)
        || (compareLists rot4)
        || (compareLists rot5)
        || (compareLists rot6)
        || (compareLists rot7)
    )

{-| Pattern may be rotated in 90,180 and 270 degree angles.
-}
rot90Symmetry : Symmetry state
rot90Symmetry state neighborhood {from,neighbors} =
    let
        directions : List (Neighborhood a -> a)
        directions = [.north,.northEast,.east,.southEast,.south,.southWest,.west,.northWest]
        
        rot : List (Neighborhood a -> a) -> List (Neighborhood a -> a)
        rot = subFuncRotate >> subFuncRotate

        rot1 : List (Neighborhood a -> a)
        rot1 = directions |> rot

        rot2 : List (Neighborhood a -> a)
        rot2 = rot1 |> rot

        rot3 : List (Neighborhood a -> a)
        rot3 = rot2 |> rot

        compareLists = 
            subFuncCompareLists neighbors neighborhood directions
    in
    (state == from)
    && 
    (   (compareLists directions)
        || (compareLists rot1)
        || (compareLists rot2)
        || (compareLists rot3)
    )

{-| Every possible way the neighbors might be arranged needs its own rule.
-}
noSymmetry : Symmetry state
noSymmetry state neighborhood {from,neighbors} =
    let
        directions : List (Neighborhood a -> a)
        directions = [.north,.northEast,.east,.southEast,.south,.southWest,.west,.northWest]

        compareLists = 
            subFuncCompareLists neighbors neighborhood directions
    in
    (state == from) && (compareLists directions)

{-| This is the main function.  
It has a wierd type, but thats because it is meant to be used with Dict.update:

    List.range 0 12
    |> List.foldl
        (\x g ->
            List.range 0 10
            |> List.foldl
                (\y -> Dict.update (x,y) ((x,y) |> step automata grid))
                g
        )
        grid
-}
step : GridAutomata state -> Grid state -> (Location-> (Maybe state) -> Maybe state)
step = CellAutomata.step