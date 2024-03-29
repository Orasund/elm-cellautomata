module CellAutomata.LifeLike exposing
    ( Grid, State(..), Position
    , AliveNeighbors(..), step, Automata, automata
    , RuleExpression(..), Neighborhood, anyNeighborhood, Rule
    , automataWithoutSymmetry
    , Symmetry, fullSymmetry, noSymmetry, horMirrorSymmetry, vertMirrorSymmetry, rot45Symmetry, rot90Symmetry
    , automataWithCustomSymmetry
    )

{-| DEPRECATED. USE CellAutomata.New with CellAuomata.Grid and CellAutomata.Rule instead.

This module was created to give a smooth introduction to the main module,
but also because for a lot of use cases this simpler version is already enough.

**If you know of Conway's Game of Life and want to make something similiar,**
**this module is the right one for you.**

In this module, a few assumptions about the automata were made:

  - The cells are organized in a two dimensional grid.
  - Each cell can have one of two states: _Dead_(`Nothing`) or _Alive_(`Just Alive`)
  - Each cell has eight neighbors
  - A rule can only take the amount of alive neighbored cells into account. (Not the pattern itself)

**Note:** This last assumption can be ignored by using `automataWithoutSymmetry`
or `automataWithCustomSymmetry`. We will look at an example further down the document.


# The Basics


## Basic Types

@docs Grid, State, Position


## Basic Automata

@docs AliveNeighbors, step, Automata, automata


# Rule Expressions and Symmetries

Up until now, only the amount of living neighbors was important, but not their position.

For the remaining documentation we use a [modified version](https://orasund.github.io/elm-cellautomata/modGoL) of game of life,
where only the four direct neighbors (North,South,East,West) are considered.


## Rule

@docs RuleExpression, Neighborhood, anyNeighborhood, Rule


## Automata

@docs automataWithoutSymmetry


## Symmetry

@docs Symmetry, fullSymmetry, noSymmetry, horMirrorSymmetry, vertMirrorSymmetry, rot45Symmetry, rot90Symmetry


## Automata with Symmetry

@docs automataWithCustomSymmetry

-}

import CellAutomata.General as General
import Dict exposing (Dict)


{-| The State will specify all posible states a cell can be in (besides a empty Cell)

This means cells will be of type `Maybe State`.
This way it should be clear that, the default value for a cell is `Nothing`.
In this module there is just one other state: `Just Alive`.
If you want to add more states, then you should go to the main module.

-}
type State
    = Alive


order : Maybe State -> Int
order maybeState =
    case maybeState of
        Nothing ->
            0

        Just Alive ->
            1


{-| The position is the unique identifier for any cell.
For our purpose we use `(x,y)`-coordinates.

**Note:** The south of `(0,0)` is `(0,y)` while the north is `(0,-y)`.

-}
type alias Position =
    ( Int, Int )


{-| The grid is the "model" of this module.

You might want to write your own view function for it or else you can't see what the automata has done.

In your head you should think of this as a grid, where some cells are filled in.
In fact, only the filled cells are stored in the `Dict`.
Filled cells have the value `Just Alive` while empty cells have the value `Nothing`.
This is why we represent the grid as a dictionary.

-}
type alias Grid =
    Dict Position State


{-| RuleExpressions give us a very flexible way of talking about neighbors.

When writing a rule for the neighbors, they can now have one of the following values:

  - `(Exactly <| Just Alive)` - its alive
  - `(Exactly <| Nothing)` - its dead
  - `(OneOf [ Just Alive , Nothing ]` - either alive or dead (but nothing else)
  - `Anything` - it may be dead or alive or something else - we don't care.

**Note:**
If you would go back to counting the alive neighbors, the `Anything`-expression will
act like an optional neighbor.
For example a rule that looks for 3 `Alive` and 2 `Anything`
will be successfull if it finds either 3,4 or 5 alive neighbors.

-}
type RuleExpression state
    = Exactly state
    | Anything


{-| This replaces the `AliveNeighbors` type.

Instead of saying "one alive neighbor", we now need to explicitly specify where
this neighbor is located.

If some neighbor may have any value (that is most often the case),
its best to use the `anyNeighborhood` template and start from there.

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

    { anyNeighborhood
        | north = Exactly a
        , east = Exactly b
        , south = Exactly c
        , west = Exactly d
    }

-}
anyNeighborhood : Neighborhood (RuleExpression (Maybe State))
anyNeighborhood =
    { north = Anything
    , northEast = Anything
    , east = Anything
    , southEast = Anything
    , south = Anything
    , southWest = Anything
    , west = Anything
    , northWest = Anything
    }


{-| This type specifies how many neighbors may be alive.
-}
type AliveNeighbors
    = AllDead
    | OneAlive
    | TwoAlive
    | ThreeAlive
    | FourAlive
    | FiveAlive
    | SixAlive
    | SevenAlive
    | EightAlive
    | AnyAmount


createNeighborhood : AliveNeighbors -> Neighborhood (RuleExpression (Maybe State))
createNeighborhood neighbors =
    case neighbors of
        AllDead ->
            { north = Exactly <| Nothing
            , northEast = Exactly <| Nothing
            , east = Exactly <| Nothing
            , southEast = Exactly <| Nothing
            , south = Exactly <| Nothing
            , southWest = Exactly <| Nothing
            , west = Exactly <| Nothing
            , northWest = Exactly <| Nothing
            }

        OneAlive ->
            { north = Exactly <| Just Alive
            , northEast = Exactly <| Nothing
            , east = Exactly <| Nothing
            , southEast = Exactly <| Nothing
            , south = Exactly <| Nothing
            , southWest = Exactly <| Nothing
            , west = Exactly <| Nothing
            , northWest = Exactly <| Nothing
            }

        TwoAlive ->
            { north = Exactly <| Just Alive
            , northEast = Exactly <| Just Alive
            , east = Exactly <| Nothing
            , southEast = Exactly <| Nothing
            , south = Exactly <| Nothing
            , southWest = Exactly <| Nothing
            , west = Exactly <| Nothing
            , northWest = Exactly <| Nothing
            }

        ThreeAlive ->
            { north = Exactly <| Just Alive
            , northEast = Exactly <| Just Alive
            , east = Exactly <| Just Alive
            , southEast = Exactly <| Nothing
            , south = Exactly <| Nothing
            , southWest = Exactly <| Nothing
            , west = Exactly <| Nothing
            , northWest = Exactly <| Nothing
            }

        FourAlive ->
            { north = Exactly <| Just Alive
            , northEast = Exactly <| Just Alive
            , east = Exactly <| Just Alive
            , southEast = Exactly <| Just Alive
            , south = Exactly <| Nothing
            , southWest = Exactly <| Nothing
            , west = Exactly <| Nothing
            , northWest = Exactly <| Nothing
            }

        FiveAlive ->
            { north = Exactly <| Just Alive
            , northEast = Exactly <| Just Alive
            , east = Exactly <| Just Alive
            , southEast = Exactly <| Just Alive
            , south = Exactly <| Just Alive
            , southWest = Exactly <| Nothing
            , west = Exactly <| Nothing
            , northWest = Exactly <| Nothing
            }

        SixAlive ->
            { north = Exactly <| Just Alive
            , northEast = Exactly <| Just Alive
            , east = Exactly <| Just Alive
            , southEast = Exactly <| Just Alive
            , south = Exactly <| Just Alive
            , southWest = Exactly <| Just Alive
            , west = Exactly <| Nothing
            , northWest = Exactly <| Nothing
            }

        SevenAlive ->
            { north = Exactly <| Just Alive
            , northEast = Exactly <| Just Alive
            , east = Exactly <| Just Alive
            , southEast = Exactly <| Just Alive
            , south = Exactly <| Just Alive
            , southWest = Exactly <| Just Alive
            , west = Exactly <| Just Alive
            , northWest = Exactly <| Nothing
            }

        EightAlive ->
            { north = Exactly <| Just Alive
            , northEast = Exactly <| Just Alive
            , east = Exactly <| Just Alive
            , southEast = Exactly <| Just Alive
            , south = Exactly <| Just Alive
            , southWest = Exactly <| Just Alive
            , west = Exactly <| Just Alive
            , northWest = Exactly <| Just Alive
            }

        AnyAmount ->
            { north = Anything
            , northEast = Anything
            , east = Anything
            , southEast = Anything
            , south = Anything
            , southWest = Anything
            , west = Anything
            , northWest = Anything
            }


{-| A rule now needs a Neighborhood instead of an `AliveNeighbors`-value.
-}
type alias Rule =
    { from : Maybe State
    , neighbors : Neighborhood (RuleExpression (Maybe State))
    , to : Maybe State
    }


{-| A symmetry is just a function that determines if a rule is sucessfully applied.
If so, it returns the new state.
During this documentation we have already encountered two different symmetries:
`fullSymmetry` and `noSymmetry`.
-}
type alias Symmetry =
    Maybe State -> Neighborhood (Maybe State) -> Rule -> Maybe (Maybe State)


{-| The position of the neighbors is not important, only the amount.
-}
fullSymmetry : Symmetry
fullSymmetry state neighborhood { from, neighbors, to } =
    let
        ( ruleSum, ruleRem ) =
            [ neighbors.north
            , neighbors.northEast
            , neighbors.east
            , neighbors.southEast
            , neighbors.south
            , neighbors.southWest
            , neighbors.west
            , neighbors.northWest
            ]
                |> List.foldl
                    (\expression ( s, balance ) ->
                        case expression of
                            Exactly a ->
                                ( s + order a, balance )

                            Anything ->
                                ( s, balance + 1 )
                    )
                    ( 0, 0 )

        sum : Int
        sum =
            [ neighborhood.north
            , neighborhood.northEast
            , neighborhood.east
            , neighborhood.southEast
            , neighborhood.south
            , neighborhood.southWest
            , neighborhood.west
            , neighborhood.northWest
            ]
                |> List.foldl
                    (\a s ->
                        s + order a
                    )
                    0
    in
    if
        (state == from)
            && (sum == ruleSum || (sum > ruleSum && sum <= ruleSum + ruleRem))
    then
        Just to

    else
        Nothing


subFuncCompareLists :
    Neighborhood (RuleExpression (Maybe State))
    -> Neighborhood (Maybe State)
    -> List (Neighborhood (RuleExpression (Maybe State)) -> RuleExpression (Maybe State))
    -> List (Neighborhood (Maybe State) -> Maybe State)
    -> Bool
subFuncCompareLists neighbors neighborhood directions dirList =
    let
        compare : RuleExpression (Maybe State) -> Maybe State -> Bool
        compare expression b =
            case expression of
                Exactly a ->
                    a == b

                Anything ->
                    True
    in
    List.map2
        (\dir1 dir2 -> compare (dir1 neighbors) (dir2 neighborhood))
        directions
        dirList
        |> List.all identity


subFuncRotate : List (Neighborhood a -> a) -> List (Neighborhood a -> a)
subFuncRotate list =
    case list of
        [] ->
            []

        a :: tail ->
            List.append tail [ a ]


{-| Pattern may be horizontally mirrored.
-}
horMirrorSymmetry : Symmetry
horMirrorSymmetry state neighborhood { from, neighbors, to } =
    let
        directions : List (Neighborhood a -> a)
        directions =
            [ .north, .northEast, .east, .southEast, .south, .southWest, .west, .northWest ]

        mirrored : List (Neighborhood a -> a)
        mirrored =
            [ .south, .southEast, .east, .northEast, .north, .northWest, .west, .southWest ]

        compareLists =
            subFuncCompareLists neighbors neighborhood directions
    in
    if
        (state == from)
            && (compareLists directions
                    || compareLists mirrored
               )
    then
        Just to

    else
        Nothing


{-| Pattern may be vertically mirrored.
-}
vertMirrorSymmetry : Symmetry
vertMirrorSymmetry state neighborhood { from, neighbors, to } =
    let
        directions : List (Neighborhood a -> a)
        directions =
            [ .north, .northEast, .east, .southEast, .south, .southWest, .west, .northWest ]

        mirrored : List (Neighborhood a -> a)
        mirrored =
            [ .north, .northWest, .west, .southWest, .south, .southEast, .east, .northEast ]

        compareLists =
            subFuncCompareLists neighbors neighborhood directions
    in
    if
        (state == from)
            && (compareLists directions
                    || compareLists mirrored
               )
    then
        Just to

    else
        Nothing


{-| Pattern may be rotated in any position.
-}
rot45Symmetry : Symmetry
rot45Symmetry state neighborhood { from, neighbors, to } =
    let
        directions : List (Neighborhood a -> a)
        directions =
            [ .north, .northEast, .east, .southEast, .south, .southWest, .west, .northWest ]

        rot : List (Neighborhood a -> a) -> List (Neighborhood a -> a)
        rot =
            subFuncRotate

        rot1 : List (Neighborhood a -> a)
        rot1 =
            directions |> rot

        rot2 : List (Neighborhood a -> a)
        rot2 =
            rot1 |> rot

        rot3 : List (Neighborhood a -> a)
        rot3 =
            rot2 |> rot

        rot4 : List (Neighborhood a -> a)
        rot4 =
            rot3 |> rot

        rot5 : List (Neighborhood a -> a)
        rot5 =
            rot4 |> rot

        rot6 : List (Neighborhood a -> a)
        rot6 =
            rot5 |> rot

        rot7 : List (Neighborhood a -> a)
        rot7 =
            rot6 |> rot

        compareLists =
            subFuncCompareLists neighbors neighborhood directions
    in
    if
        (state == from)
            && (compareLists directions
                    || compareLists rot1
                    || compareLists rot2
                    || compareLists rot3
                    || compareLists rot4
                    || compareLists rot5
                    || compareLists rot6
                    || compareLists rot7
               )
    then
        Just to

    else
        Nothing


{-| Pattern may be rotated in 90,180 and 270 degree angles.
-}
rot90Symmetry : Symmetry
rot90Symmetry state neighborhood { from, neighbors, to } =
    let
        directions : List (Neighborhood a -> a)
        directions =
            [ .north, .northEast, .east, .southEast, .south, .southWest, .west, .northWest ]

        rot : List (Neighborhood a -> a) -> List (Neighborhood a -> a)
        rot =
            subFuncRotate >> subFuncRotate

        rot1 : List (Neighborhood a -> a)
        rot1 =
            directions |> rot

        rot2 : List (Neighborhood a -> a)
        rot2 =
            rot1 |> rot

        rot3 : List (Neighborhood a -> a)
        rot3 =
            rot2 |> rot

        compareLists =
            subFuncCompareLists neighbors neighborhood directions
    in
    if
        (state == from)
            && (compareLists directions
                    || compareLists rot1
                    || compareLists rot2
                    || compareLists rot3
               )
    then
        Just to

    else
        Nothing


{-| Every possible way the neighbors might be arranged needs its own rule.
-}
noSymmetry : Symmetry
noSymmetry state neighborhood { from, neighbors, to } =
    let
        directions : List (Neighborhood a -> a)
        directions =
            [ .north, .northEast, .east, .southEast, .south, .southWest, .west, .northWest ]

        compareLists =
            subFuncCompareLists neighbors neighborhood directions
    in
    if (state == from) && compareLists directions then
        Just to

    else
        Nothing


type alias NeighborhoodFunction =
    Position -> Grid -> Neighborhood (Maybe State)


neighborhoodFunction : NeighborhoodFunction
neighborhoodFunction ( x, y ) grid =
    { north = grid |> Dict.get ( x, y - 1 )
    , northEast = grid |> Dict.get ( x + 1, y - 1 )
    , east = grid |> Dict.get ( x + 1, y )
    , southEast = grid |> Dict.get ( x + 1, y + 1 )
    , south = grid |> Dict.get ( x, y + 1 )
    , southWest = grid |> Dict.get ( x - 1, y + 1 )
    , west = grid |> Dict.get ( x - 1, y )
    , northWest = grid |> Dict.get ( x - 1, y - 1 )
    }


{-| The `Automata` type can be seen as a config type.
Its stores all information to specify the behaviour of a cell automata.
Sometimes more then one automata should act on to the same Grid.
For this reason it is its own type.
-}
type alias Automata =
    General.Automata (Neighborhood (Maybe State)) (Neighborhood (RuleExpression (Maybe State))) Position State Int


{-| The input is a list of rules.
[As an example](https://orasund.github.io/elm-cellautomata/GameOfLife), lets look at the rules for conway's game of life:

  - Alive cells survive if they have 2 or 3 alive neighbors, else they die.
  - Dead cells turn alive if exactly 3 neighbors are alive.

Implemented, these rules look like this:

    [ { from = Just Alive
      , neighbors = TwoAlive
      , to = Just Alive
      }
    , { from = Just Alive
      , neighbors = ThreeAlive
      , to = Just Alive
      }
    , { from = Just Alive
      , neighbors = AnyAmount
      , to = Nothing
      }
    , { from = Nothing
      , neighbors = ThreeAlive
      , to = Just Alive
      }
    ]
        |> automata

The order of the rules are important: the automata will go through the list,
and use the first rule it can apply.

-}
automata : List { from : Maybe State, neighbors : AliveNeighbors, to : Maybe State } -> Automata
automata =
    List.map
        (\{ from, neighbors, to } ->
            { from = from, neighbors = neighbors |> createNeighborhood, to = to }
        )
        >> automataWithCustomSymmetry fullSymmetry


{-| This function uses no symmetry. This means every possible combination must be
specified.

    automataWithoutSymmetry =
        automataWithCustomSymmetry noSymmetry

**This function is not useful in practice.**
**Most often you want at least rotational or mirrored symmetry**
**This function is only included for demonstration purposes**

[For example](https://orasund.github.io/elm-cellautomata/modGoL), lets say we want to modify conway's game of life, such that
it only considers the four adjacent neighbors:

  - Alive cells survive if they have exactly 1 adjacent neighbors
  - Dead cells turn alive if exactly 1 neighbors are alive

The implementation would be the following list:

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
    , { from = Just Alive
      , to = Just Alive
      , neighbors = neighbors Nothing (Just Alive) Nothing Nothing
      }
    , { from = Just Alive
      , to = Just Alive
      , neighbors = neighbors Nothing Nothing (Just Alive) Nothing
      }
    , { from = Just Alive
      , to = Just Alive
      , neighbors = neighbors Nothing Nothing Nothing (Just Alive)
      }
    , { from = Just Alive, to = Nothing, neighbors = anyNeighborhood }
    , { from = Nothing
      , to = Just Alive
      , neighbors = neighbors (Just Alive) Nothing Nothing Nothing
      }
    , { from = Nothing
      , to = Just Alive
      , neighbors = neighbors Nothing (Just Alive) Nothing Nothing
      }
    , { from = Nothing
      , to = Just Alive
      , neighbors = neighbors Nothing Nothing (Just Alive) Nothing
      }
    , { from = Nothing
      , to = Just Alive
      , neighbors = neighbors Nothing Nothing Nothing (Just Alive)
      }
    ]

As one can tell, that example blew up. Thats because we did not use any symmetry.

-}
automataWithoutSymmetry : List Rule -> Automata
automataWithoutSymmetry =
    automataWithCustomSymmetry noSymmetry


{-| With this function we can now add our own symmetry.

Going back to the old example, this can now be done the following way:

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
    , { from = Just Alive
      , to = Nothing
      , neighbors = anyNeighborhood
      }
    , { from = Nothing
      , to = Just Alive
      , neighbors = neighbors (Just Alive) Nothing Nothing Nothing
      }
    ]
        |> automataWithCustomSymmetry rot90Symmetry

-}
automataWithCustomSymmetry : Symmetry -> List Rule -> Automata
automataWithCustomSymmetry symmetry listOfRules =
    General.Automata
        { ruleSet =
            listOfRules
                |> General.ruleSet order
        , symmetry = symmetry
        , neighborhoodFunction = neighborhoodFunction
        , order = order
        }


{-| This is the main function.
It has a wierd type, but thats because it is meant to be used with \``Dict.update`:

    List.range 0 12
        |> List.foldl
            (\x g ->
                List.range 0 10
                    |> List.foldl
                        (\y ->
                            Dict.update
                                ( x, y )
                                (( x, y ) |> step automata grid)
                        )
                        g
            )
            grid

-}
step : Automata -> Grid -> (Position -> Maybe State -> Maybe State)
step =
    General.step
