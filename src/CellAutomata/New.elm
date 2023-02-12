module CellAutomata.New exposing (CellAutomata, new, step, withGroups)

{-| This module is the new version of the cell automata package.

@docs CellAutomata, new, step, withGroups

-}

import CellAutomata.Rule exposing (Rule)
import Dict exposing (Dict)


{-| This type contains the needed information to compute the next step of an automata.
-}
type alias CellAutomata comparable state =
    { rules : Dict String (List (Rule comparable state))
    , neighbors : comparable -> List { location : comparable, direction : comparable }
    , groups : Maybe state -> String
    }


{-| Construct a new cell automata
-}
new : { rules : List (Rule comparable state), neighbors : comparable -> List { location : comparable, direction : comparable } } -> CellAutomata comparable state
new args =
    { rules = Dict.singleton "" args.rules
    , neighbors = args.neighbors
    , groups = always ""
    }


{-| If you have a lot of rules, it might be good to group them by initial state. To do so, you have to provide a function that map the states to the names of the groups.
-}
withGroups : (Maybe state -> String) -> CellAutomata comparable state -> CellAutomata comparable state
withGroups groups aut =
    { aut
        | rules =
            aut.rules
                |> Dict.values
                |> List.concat
                |> List.foldr
                    (\({ from } as r) dict ->
                        dict
                            |> Dict.update
                                (from |> groups)
                                (\maybeList ->
                                    case maybeList of
                                        Nothing ->
                                            Just [ r ]

                                        Just list ->
                                            Just (r :: list)
                                )
                    )
                    Dict.empty
        , groups = groups
    }


{-| Compute a step using a cell automata
-}
step : CellAutomata comparable state -> Dict comparable state -> comparable -> Maybe state -> Maybe state
step args dict =
    \location state ->
        let
            neighborhood : Dict comparable state
            neighborhood =
                args.neighbors location
                    |> List.filterMap
                        (\k ->
                            dict
                                |> Dict.get k.location
                                |> Maybe.map (Tuple.pair k.direction)
                        )
                    |> Dict.fromList
        in
        args.rules
            |> Dict.get (args.groups state)
            |> Maybe.withDefault []
            |> internalFindMap
                (\rule ->
                    if rule.from == state && rule.neighborhood neighborhood then
                        rule.to
                            |> Just

                    else
                        Nothing
                )
            |> Maybe.withDefault state


internalFindMap : (a -> Maybe b) -> List a -> Maybe b
internalFindMap predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case predicate first of
                Just out ->
                    Just out

                Nothing ->
                    internalFindMap predicate rest
