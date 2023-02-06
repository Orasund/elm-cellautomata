module CellAutomata.Grid exposing (..)


type alias Direction =
    ( Int, Int )


adjacentDirections : List Direction
adjacentDirections =
    [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]


surroundingDirections : List Direction
surroundingDirections =
    [ ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 1 ) ]
        ++ adjacentDirections
