module CellAutomata.Advanced exposing (..)

import CellAutomata.Rule exposing (Rule)
import Dict exposing (Dict)


type alias Automata location state =
    { rules : Dict String (List (Rule location state))
    , neighbors : location -> List location
    , groups : Maybe state -> String
    }


automata : { rules : List (Rule location state), neighbors : location -> List location } -> Automata location state
automata args =
    { rules = Dict.singleton "" args.rules
    , neighbors = args.neighbors
    , groups = always ""
    }


withGroups : (Maybe state -> String) -> Automata location state -> Automata location state
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


step : Automata comparableLocation state -> Dict comparableLocation state -> (comparableLocation -> Maybe state -> Maybe state)
step args dict =
    \location state ->
        let
            neighborhood : Dict comparableLocation state
            neighborhood =
                args.neighbors location
                    |> List.filterMap (\k -> dict |> Dict.get k |> Maybe.map (Tuple.pair k))
                    |> Dict.fromList
        in
        args.rules
            |> Dict.get (args.groups state)
            |> Maybe.withDefault []
            |> internalFindMap
                (\rule ->
                    if rule.neighborhood neighborhood then
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
