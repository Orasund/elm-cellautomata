module CellAutomata exposing
    ( Order, Grid, Position
    , RuleExpression(..), Neighborhood, anyNeighborhood, Rule
    , step, Automata, automataWithoutSymmetry
    , Symmetry, noSymmetry, horMirrorSymmetry, vertMirrorSymmetry, rot45Symmetry, rot90Symmetry
    , automata
    , mapNeighborhood
    )

{-| DEPRECATED. USE CellAutomata.New instead.

If you are new to this package, consider checking out CellAutomata.LifeLike first.
It is written as an introduction to this module.

**If you want to create automatas with more then one state,**
**but that still operates on a 2D-Grid, than this package is the right one for you.**

First start by writing your own state type.
[As an example](https://orasund.github.io/elm-cellautomata/ant), lets try to simulate an ant that always follows the right wall.

Our state will now be the following

    type State
        = Wall
        | Up
        | Down
        | Left
        | Right


# The Basics


## Types

@docs Order, Grid, Position


## Rule

@docs RuleExpression, Neighborhood, anyNeighborhood, Rule


## Automata Without Symmetry

@docs step, Automata, automataWithoutSymmetry


# Symmetries


## Symmetry

@docs Symmetry, noSymmetry, horMirrorSymmetry, vertMirrorSymmetry, rot45Symmetry, rot90Symmetry


## Automata

@docs automata


## Helpful sub-functions

@docs mapNeighborhood

-}

import CellAutomata.General as General
import Dict exposing (Dict)


{-| Every state needs a defined order.
(A function that gives each state a unique identifier)

For our ant example we define the following order:

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

-}
type alias Order state comparable =
    Maybe state -> comparable


{-| The position is the unique identifier for any cell.
For our purpose we use `(x,y)` coordinates.

**Note:** The south of `(0,0)` is `(0,y)` while the north is `(0,-y)`.

-}
type alias Position =
    ( Int, Int )


{-| The grid is the _model_ of this module.

You might want to write your own view function for it
or else you can't see what the automata has done.

In your head you should think of this as a grid, where some cells are filled in.
In fact, only the filled cells are stored in the `Dict`.

Cells have the type `Maybe state` where `state` should be a custom type.

-}
type alias Grid state =
    Dict Position state


{-| RuleExpressions give us a very flexible way of talking about neighbors.

Saying something is `Anything`, it means its value is ignored.

-}
type RuleExpression state
    = Exactly state
    | OneOf (List state)
    | Anything


{-| The Neighborhood of a cell consists of the 8 surounding cells.

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


{-| Transforms a neighborhood.

It takes a function `a -> b` in order to transform a `Neighborhood a` to `Neighborhood b`

-}
mapNeighborhood : (a -> b) -> Neighborhood a -> Neighborhood b
mapNeighborhood fun { north, northEast, east, southEast, south, southWest, west, northWest } =
    { north = north |> fun
    , northEast = northEast |> fun
    , east = east |> fun
    , southEast = southEast |> fun
    , south = south |> fun
    , southWest = southWest |> fun
    , west = west |> fun
    , northWest = northWest |> fun
    }


{-| This template helps defining a `Neighborhood`.

For example, if we would want to only consider the north neighbor,
we might specify it the following way

    { anyNeighborhood
        | north = Exactly a
    }

-}
anyNeighborhood : Neighborhood (RuleExpression (Maybe state))
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


{-| A rule consist of the following elements:

  - **from** - The state of the cell
  - **neighbors** - The nessesary pattern of the neighbors
  - **to** - The state, the cell will transition to, once the rule applies

-}
type alias Rule state =
    { from : Maybe state
    , neighbors : Neighborhood (RuleExpression (Maybe state))
    , to : Maybe state
    }


type alias RuleSet state comparable =
    General.RuleSet (Neighborhood (RuleExpression (Maybe state))) (Maybe state) comparable


ruleSet : Dict comparable (List (Rule state)) -> RuleSet state comparable
ruleSet =
    General.RuleSet


{-| The Automata type can be seen as a config type.

Its stores all information to specify the behaviour of a cell automata.
Sometimes more then one automata should act on to the same `Grid`.
For this reason it is its own type.

-}
type alias Automata state comparable =
    General.Automata (Neighborhood (Maybe state)) (Neighborhood (RuleExpression (Maybe state))) Position state comparable


{-| A symmetry is just a function that determines if a rule is sucessfully applied.
If so, it returns the new state.
-}
type alias Symmetry state =
    Maybe state -> Neighborhood (Maybe state) -> Rule state -> Maybe (Maybe state)


{-| This function uses no symmetry, this means every possible combination must be
specified.

    automataWithoutSymmetry =
        automataWithCustomSymmetry noSymmetry

**This function is not useful in practice.**
**Most often you want at least rotational or mirrored symmetry.**
**This function is only included for demonstration purposes.**

Checkout [CellAutomata.LifeLike](https://package.elm-lang.org/packages/Orasund/elm-cellautomata/latest/CellAutomata-LifeLike) for a more detailed description.

Our example with the ant, can now be implemented the following way:

    [ { from = Just Up, neighbors = anyNeighborhood, to = Nothing }
    , { from = Just Down, neighbors = anyNeighborhood, to = Nothing }
    , { from = Just Left, neighbors = anyNeighborhood, to = Nothing }
    , { from = Just Right, neighbors = anyNeighborhood, to = Nothing }
    , { from = Nothing, to = Just Up, neighbors = { anyNeighborhood | south = Exactly <| Just Up, southEast = Exactly <| Just Wall } }
    , { from = Nothing, to = Just Right, neighbors = { anyNeighborhood | south = Exactly <| Just Wall, west = Exactly <| Just Up } }
    , { from = Nothing, to = Just Right, neighbors = { anyNeighborhood | west = Exactly <| Just Right, southWest = Exactly <| Just Wall } }
    , { from = Nothing, to = Just Down, neighbors = { anyNeighborhood | west = Exactly <| Just Wall, north = Exactly <| Just Right } }
    , { from = Nothing, to = Just Down, neighbors = { anyNeighborhood | north = Exactly <| Just Right, northWest = Exactly <| Just Wall } }
    , { from = Nothing, to = Just Left, neighbors = { anyNeighborhood | north = Exactly <| Just Wall, east = Exactly <| Just Down } }
    , { from = Nothing, to = Just Left, neighbors = { anyNeighborhood | east = Exactly <| Just Right, northEast = Exactly <| Just Wall } }
    , { from = Nothing, to = Just Up, neighbors = { anyNeighborhood | east = Exactly <| Just Wall, south = Exactly <| Just Left } }
    ]
        |> automataWithoutSymmetry order

This code is on purpose more complicated as it should be.
If possible one should always use a custom symmetry.

-}
automataWithoutSymmetry : Order state comparable -> List (Rule state) -> Automata state comparable
automataWithoutSymmetry =
    automata noSymmetry


{-| With this function we can now add our own symmetry.

The ant example can now be implemented the following way:

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
    List.concat
        [ [ Up, Left, Right, Down ]
            |> List.map
                (\dir ->
                    { from = Just dir
                    , neighbors = anyNeighborhood
                    , to = Nothing
                    }
                )
        , [ { from = Nothing
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
                    , west = Exactly <| Just Up
                }
            }
          , { from = Nothing
            , to = Just Right
            , neighbors =
                { anyNeighborhood
                    | southWest = Exactly <| Just Wall
                    , west = Exactly <| Just Down
                }
            }
          ]
        ]
        |> automata (rot90Symmetry rotState) order

-}
automata : Symmetry state -> Order state comparable -> List (Rule state) -> Automata state comparable
automata symmetry order listOfRules =
    General.Automata
        { ruleSet =
            listOfRules
                |> General.ruleSet order
        , symmetry = symmetry
        , neighborhoodFunction = neighborhoodFunction
        , order = order
        }


type alias NeighborhoodFunction state =
    Position -> Grid state -> Neighborhood (Maybe state)


neighborhoodFunction : NeighborhoodFunction state
neighborhoodFunction ( x, y ) field =
    { north = field |> Dict.get ( x, y - 1 )
    , northEast = field |> Dict.get ( x + 1, y - 1 )
    , east = field |> Dict.get ( x + 1, y )
    , southEast = field |> Dict.get ( x + 1, y + 1 )
    , south = field |> Dict.get ( x, y + 1 )
    , southWest = field |> Dict.get ( x - 1, y + 1 )
    , west = field |> Dict.get ( x - 1, y )
    , northWest = field |> Dict.get ( x - 1, y - 1 )
    }


subFuncCompareLists :
    Neighborhood (RuleExpression (Maybe state))
    -> Neighborhood (Maybe state)
    -> List (Neighborhood (RuleExpression (Maybe state)) -> RuleExpression (Maybe state))
    -> List (Neighborhood (Maybe state) -> Maybe state)
    -> Bool
subFuncCompareLists neighbors neighborhood directions dirList =
    let
        compare : RuleExpression (Maybe state) -> Maybe state -> Bool
        compare expression b =
            case expression of
                Exactly a ->
                    a == b

                OneOf list ->
                    list |> List.any ((==) b)

                Anything ->
                    True
    in
    List.map2
        (\dir1 dir2 -> compare (dir1 neighbors) (dir2 neighborhood))
        directions
        dirList
        |> List.all identity


subFuncRotate : List a -> List a
subFuncRotate list =
    case list of
        [] ->
            []

        a :: tail ->
            List.append tail [ a ]


{-| Pattern may be horizontally mirrored

The first argument is a function `(state -> state)` that states how the values of
the state can be mirrored (horizontally).
Use the `identity` function if you do not see a need in specifing the first arguement.

As example, given the state

    State = Up
        | Down
        | Left
        | Right

We can specify a symmetry the following way

    horMirrorSymmetry
        (\state ->
            case state of
                Up ->
                    Down

                Down ->
                    Up

                a ->
                    a
        )

-}
horMirrorSymmetry : (state -> state) -> Symmetry state
horMirrorSymmetry horMirrorState state neighborhood { from, neighbors, to } =
    let
        directions : List (Neighborhood a -> a)
        directions =
            [ .north, .northEast, .east, .southEast, .south, .southWest, .west, .northWest ]

        mirrored : List (Neighborhood a -> a)
        mirrored =
            [ .south, .southEast, .east, .northEast, .north, .northWest, .west, .southWest ]

        compareLists neigh =
            subFuncCompareLists neighbors neigh directions
    in
    if (state == from) && compareLists neighborhood directions then
        Just to

    else if (state == from) && compareLists (neighborhood |> mapNeighborhood (Maybe.map horMirrorState)) mirrored then
        Just (to |> Maybe.map horMirrorState)

    else
        Nothing


{-| Pattern may be vertically mirrored

The first argument is a function (state -> state) that states how the values of
the state can be mirrored (vertically).
Use the identity function if you dont see a need in specifing the first arguement.

As example, given the state

    State = Up
        | Down
        | Left
        | Right

We can specify the symmetry the following way

    vertMirrorSymmetry
        (\state ->
            case state of
                Left ->
                    Right

                Right ->
                    Left

                a ->
                    a
        )

-}
vertMirrorSymmetry : (state -> state) -> Symmetry state
vertMirrorSymmetry vertMirrorState state neighborhood { from, neighbors, to } =
    let
        directions : List (Neighborhood a -> a)
        directions =
            [ .north, .northEast, .east, .southEast, .south, .southWest, .west, .northWest ]

        mirrored : List (Neighborhood a -> a)
        mirrored =
            [ .north, .northWest, .west, .southWest, .south, .southEast, .east, .northEast ]

        compareLists neigh =
            subFuncCompareLists neighbors neigh directions
    in
    if (state == from) && compareLists neighborhood directions then
        Just to

    else if
        (state == from)
            && compareLists (neighborhood |> mapNeighborhood (Maybe.map vertMirrorState)) mirrored
    then
        Just (to |> Maybe.map vertMirrorState)

    else
        Nothing


{-| Pattern may be rotated in any position.

The first argument is a function `(state -> state)` that states how the values of
the state can be rotated (clock-wise).
Use the `identity` function if you dont see a need in specifing the first arguement.

As example, given the state

    State = North
        | NorthEast
        | East
        | SouthEast
        | South
        | SouthWest
        | West
        | NorthWest

We can specify the symmetry the following way

    rot45Symmetry
        (\state ->
            case state of
                North ->
                    NorthEast

                NorthEast ->
                    East

                East ->
                    SouthEast

                SouthEast ->
                    South

                South ->
                    SouthWest

                SouthWest ->
                    West

                South ->
                    NorthWest

                NorthWest ->
                    North
        )

-}
rot45Symmetry : (state -> state) -> Symmetry state
rot45Symmetry rotateState state neighborhood { from, neighbors, to } =
    let
        directions : List (Neighborhood a -> a)
        directions =
            [ .north, .northEast, .east, .southEast, .south, .southWest, .west, .northWest ]

        rot : List (Neighborhood a -> a) -> List (Neighborhood a -> a)
        rot =
            subFuncRotate

        ( rot1, neighbors1 ) =
            ( directions |> rot
            , neighborhood |> mapNeighborhood (Maybe.map rotateState)
            )

        ( rot2, neighbors2 ) =
            ( rot1 |> rot
            , neighbors1 |> mapNeighborhood (Maybe.map rotateState)
            )

        ( rot3, neighbors3 ) =
            ( rot2 |> rot
            , neighbors2 |> mapNeighborhood (Maybe.map rotateState)
            )

        ( rot4, neighbors4 ) =
            ( rot3 |> rot
            , neighbors3 |> mapNeighborhood (Maybe.map rotateState)
            )

        ( rot5, neighbors5 ) =
            ( rot4 |> rot
            , neighbors4 |> mapNeighborhood (Maybe.map rotateState)
            )

        ( rot6, neighbors6 ) =
            ( rot5 |> rot
            , neighbors5 |> mapNeighborhood (Maybe.map rotateState)
            )

        ( rot7, neighbors7 ) =
            ( rot6 |> rot
            , neighbors6 |> mapNeighborhood (Maybe.map rotateState)
            )

        compareLists neigh =
            subFuncCompareLists neighbors neigh directions
    in
    if (state == from) && compareLists neighborhood directions then
        Just to

    else if (state == from) && compareLists neighbors1 rot1 then
        Just (to |> Maybe.map (rotateState >> rotateState >> rotateState >> rotateState >> rotateState >> rotateState >> rotateState))

    else if (state == from) && compareLists neighbors2 rot2 then
        Just (to |> Maybe.map (rotateState >> rotateState >> rotateState >> rotateState >> rotateState >> rotateState))

    else if (state == from) && compareLists neighbors3 rot3 then
        Just (to |> Maybe.map (rotateState >> rotateState >> rotateState >> rotateState >> rotateState))

    else if (state == from) && compareLists neighbors4 rot4 then
        Just (to |> Maybe.map (rotateState >> rotateState >> rotateState >> rotateState))

    else if (state == from) && compareLists neighbors5 rot5 then
        Just (to |> Maybe.map (rotateState >> rotateState >> rotateState))

    else if (state == from) && compareLists neighbors6 rot6 then
        Just (to |> Maybe.map (rotateState >> rotateState))

    else if (state == from) && compareLists neighbors7 rot7 then
        Just (to |> Maybe.map rotateState)

    else
        Nothing


{-| Pattern may be rotated in 90,180 and 270 degree angles.

The first argument is a function `(state -> state)` that states how the values of
the state can be rotated (clock-wise).
Use the `identity` function if you dont see a need in specifing the first arguement.

As example, given the state

    State = Left
        | Right
        | Up
        | Down

We can specify the symmetry the following way

    rot90Symmetry
        (\state ->
            case state of
                Left ->
                    Down

                Down ->
                    Right

                Right ->
                    Up

                Up ->
                    Left
        )

-}
rot90Symmetry : (state -> state) -> Symmetry state
rot90Symmetry rotateState state neighborhood { from, neighbors, to } =
    let
        directions : List (Neighborhood a -> a)
        directions =
            [ .north, .northEast, .east, .southEast, .south, .southWest, .west, .northWest ]

        rot : List (Neighborhood a -> a) -> List (Neighborhood a -> a)
        rot =
            subFuncRotate >> subFuncRotate

        ( rot1, neighbors1 ) =
            ( directions |> rot
            , neighborhood |> mapNeighborhood (Maybe.map rotateState)
            )

        ( rot2, neighbors2 ) =
            ( rot1 |> rot
            , neighbors1 |> mapNeighborhood (Maybe.map rotateState)
            )

        ( rot3, neighbors3 ) =
            ( rot2 |> rot
            , neighbors2 |> mapNeighborhood (Maybe.map rotateState)
            )

        compareLists neigh =
            subFuncCompareLists neighbors neigh directions
    in
    if (state == from) && compareLists neighborhood directions then
        Just to

    else if (state == from) && compareLists neighbors1 rot1 then
        Just (to |> Maybe.map (rotateState >> rotateState >> rotateState))

    else if (state == from) && compareLists neighbors2 rot2 then
        Just (to |> Maybe.map (rotateState >> rotateState))

    else if (state == from) && compareLists neighbors3 rot3 then
        Just (to |> Maybe.map rotateState)

    else
        Nothing


{-| Every possible way the neighbors might be arranged needs its own rule.
-}
noSymmetry : Symmetry state
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


{-| This is the main function.
It has a wierd type, but thats because it is meant to be used with `Dict.update`:

    List.range 0 12
        |> List.foldl
            (\x g ->
                List.range 0 10
                    |> List.foldl
                        (\y ->
                            Dict.update
                                ( x, y )
                                (( x, y )
                                    |> step automata grid
                                )
                        )
                        g
            )
            grid

-}
step : Automata state comparable -> Grid state -> (Position -> Maybe state -> Maybe state)
step =
    General.step
