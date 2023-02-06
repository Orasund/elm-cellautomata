module CellAutomata.Rule exposing (Rule, fromNothing, from, withNeighborhood, to)

{-| module for building rules.

@docs Rule, fromNothing, from, withNeighborhood, to

-}

import Dict exposing (Dict)


{-| A Rule specifies how a given cell should change based on its neighbors.
-}
type alias Rule direction state =
    { from : Maybe state
    , neighborhood : Dict direction state -> Bool
    , to : Maybe state
    }


{-| Only empty cells should be effected.
-}
fromNothing : Rule direction state
fromNothing =
    { from = Nothing
    , neighborhood = always True
    , to = Nothing
    }


{-| Which cell is effected?
-}
from : state -> Rule direction state
from state =
    { fromNothing | from = Just state }


{-| How do the neighbors look like?
-}
withNeighborhood : (Dict direction state -> Bool) -> Rule direction state -> Rule direction state
withNeighborhood neighborhood rule =
    { rule
        | neighborhood = neighborhood
    }


{-| What should the cell turn into?
-}
to : state -> Rule direction state -> Rule direction state
to state rule =
    { rule | to = Just state }
