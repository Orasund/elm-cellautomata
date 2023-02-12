module CellAutomata.Grid exposing (surroundingNeighbors, touchingNeighbors, diagonalNeighbors)

{-| This module should help you work with automatas on a grid.

@docs surroundingNeighbors, touchingNeighbors, diagonalNeighbors

-}


{-| Compute the 4 direct neighbors
-}
touchingNeighbors : ( Int, Int ) -> List { location : ( Int, Int ), direction : ( Int, Int ) }
touchingNeighbors ( x, y ) =
    [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
        |> List.map
            (\dir ->
                { location = Tuple.mapBoth ((+) x) ((+) y) dir
                , direction = dir
                }
            )


{-| Compute the 4 diagonal neighbors
-}
diagonalNeighbors : ( Int, Int ) -> List { location : ( Int, Int ), direction : ( Int, Int ) }
diagonalNeighbors ( x, y ) =
    [ ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 1 ) ]
        |> List.map
            (\dir ->
                { location = Tuple.mapBoth ((+) x) ((+) y) dir
                , direction = dir
                }
            )


{-| Compute all 8 surrounding neighbors
-}
surroundingNeighbors : ( Int, Int ) -> List { location : ( Int, Int ), direction : ( Int, Int ) }
surroundingNeighbors ( x, y ) =
    diagonalNeighbors ( x, y ) ++ touchingNeighbors ( x, y )
