module CellAutomata.Grid exposing (..)


touchingNeighbors : ( Int, Int ) -> List { location : ( Int, Int ), direction : ( Int, Int ) }
touchingNeighbors ( x, y ) =
    [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
        |> List.map
            (\dir ->
                { location = Tuple.mapBoth ((+) x) ((+) y) dir
                , direction = dir
                }
            )


diagonalNeighbors : ( Int, Int ) -> List { location : ( Int, Int ), direction : ( Int, Int ) }
diagonalNeighbors ( x, y ) =
    [ ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 1 ) ]
        |> List.map
            (\dir ->
                { location = Tuple.mapBoth ((+) x) ((+) y) dir
                , direction = dir
                }
            )


surroundingNeighbors : ( Int, Int ) -> List { location : ( Int, Int ), direction : ( Int, Int ) }
surroundingNeighbors ( x, y ) =
    diagonalNeighbors ( x, y ) ++ touchingNeighbors ( x, y )
