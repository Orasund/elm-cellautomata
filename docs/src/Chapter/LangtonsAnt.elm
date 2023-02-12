module Chapter.LangtonsAnt exposing (..)

import CellAutomata.Advanced as CellAutomata exposing (Automata)
import CellAutomata.Grid
import CellAutomata.Rule exposing (Rule)
import Data.Editor exposing (Editor)
import Dict
import ElmBook.Chapter exposing (Chapter)
import View.Editor


type Direction
    = North
    | East
    | South
    | West


directions : List Direction
directions =
    [ North, East, South, West ]


turnLeft : Direction -> Direction
turnLeft dir =
    case dir of
        North ->
            West

        East ->
            North

        South ->
            East

        West ->
            South


turnRight : Direction -> Direction
turnRight dir =
    case dir of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


toDirection : ( Int, Int ) -> Direction
toDirection ( x, y ) =
    if x == 0 then
        if y < 0 then
            North

        else
            South

    else if x < 0 then
        West

    else
        East


fromDirection : Direction -> ( Int, Int )
fromDirection dir =
    case dir of
        North ->
            ( 0, -1 )

        East ->
            ( 1, 0 )

        South ->
            ( 0, 1 )

        West ->
            ( -1, 0 )


type State
    = Alive
    | Ant Direction Bool


moveToRule : Direction -> Bool -> Bool -> Rule ( Int, Int ) State
moveToRule dir onAlive from =
    (if from then
        CellAutomata.Rule.from Alive

     else
        CellAutomata.Rule.fromNothing
    )
        |> CellAutomata.Rule.withNeighborhood
            (\dict ->
                dir
                    |> turnLeft
                    |> turnLeft
                    |> fromDirection
                    |> (\position ->
                            Dict.get position dict
                                == Just
                                    (Ant
                                        (if onAlive then
                                            dir |> turnLeft

                                         else
                                            dir |> turnRight
                                        )
                                        onAlive
                                    )
                       )
            )
        |> CellAutomata.Rule.to (Ant dir from)


moveFromRule : Direction -> Bool -> Rule ( Int, Int ) State
moveFromRule dir onAlive =
    CellAutomata.Rule.from (Ant dir onAlive)
        |> (if onAlive then
                CellAutomata.Rule.toNothing

            else
                CellAutomata.Rule.to Alive
           )


automata : Automata ( Int, Int ) State
automata =
    CellAutomata.automata
        { rules =
            directions
                |> List.concatMap (\dir -> [ True, False ] |> List.map (Tuple.pair dir))
                |> List.concatMap
                    (\( dir, onAlive ) ->
                        ([ True, False ]
                            |> List.map (moveToRule dir onAlive)
                        )
                            ++ [ moveFromRule dir onAlive ]
                    )
        , neighbors = CellAutomata.Grid.surroundingNeighbors
        }
        |> CellAutomata.withGroups
            (\maybe ->
                case maybe of
                    Just (Ant _ True) ->
                        "onAlive"
                    
                    Just (Ant _ False) ->
                        "onDead"
                    _ ->
                        ""
            )


init : Editor State
init =
    Dict.empty
        |> Data.Editor.init
            { width = 10
            , height = 10
            , selected = Alive
            , automata = automata
            }


toString : State -> String
toString state =
    case state of
        Alive ->
            "⚫️"

        Ant _ _ ->
            "🐜"


chapter : { read : state -> Editor State, writeTo : state -> Editor State -> state } -> Chapter state
chapter args =
    ElmBook.Chapter.chapter "Langton's Ant"
        |> ElmBook.Chapter.withStatefulComponentList
            [ ( "Editor"
              , View.Editor.toHtml
                    { read = args.read
                    , writeTo = args.writeTo
                    , toString = toString
                    , placableStates = [ Alive, Ant North False ]
                    }
              )
            ]
        |> ElmBook.Chapter.render """
        
<component with-label="Editor"/>

```
type Direction
    = North
    | East
    | South
    | West

type State
    = Alive
    | Ant Direction Bool
```


```
directions : List Direction
directions =
    [ North, East, South, West ]


turnLeft : Direction -> Direction
turnLeft dir =
    case dir of
        North ->
            West

        East ->
            North

        South ->
            East

        West ->
            South


turnRight : Direction -> Direction
turnRight dir =
    case dir of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


toDirection : ( Int, Int ) -> Direction
toDirection ( x, y ) =
    if x == 0 then
        if y < 0 then
            North

        else
            South

    else if x < 0 then
        West

    else
        East


fromDirection : Direction -> ( Int, Int )
fromDirection dir =
    case dir of
        North ->
            ( 0, -1 )

        East ->
            ( 1, 0 )

        South ->
            ( 0, 1 )

        West ->
            ( -1, 0 )
```

```
moveToRule : Direction -> Bool -> Bool -> Rule ( Int, Int ) State
moveToRule dir onAlive from =
    (if from then
        CellAutomata.Rule.from Alive

     else
        CellAutomata.Rule.fromNothing
    )
        |> CellAutomata.Rule.withNeighborhood
            (\\dict ->
                dir
                    |> turnLeft
                    |> turnLeft
                    |> fromDirection
                    |> (\\position ->
                            Dict.get position dict
                                == Just
                                    (Ant
                                        (if onAlive then
                                            dir |> turnLeft

                                         else
                                            dir |> turnRight
                                        )
                                        onAlive
                                    )
                       )
            )
        |> CellAutomata.Rule.to (Ant dir from)


moveFromRule : Direction -> Bool -> Rule ( Int, Int ) State
moveFromRule dir onAlive =
    CellAutomata.Rule.from (Ant dir onAlive)
        |> (if onAlive then
                CellAutomata.Rule.toNothing

            else
                CellAutomata.Rule.to Alive
           )

automata : Automata ( Int, Int ) State
automata =
    CellAutomata.automata
        { rules =
            directions
                |> List.concatMap (\\dir -> [ True, False ] |> List.map (Tuple.pair dir))
                |> List.concatMap
                    (\\( dir, onAlive ) ->
                        ([ True, False ]
                            |> List.map (moveToRule dir onAlive)
                        )
                            ++ [ moveFromRule dir onAlive ]
                    )
        , neighbors = CellAutomata.Grid.surroundingNeighbors
        }
        |> CellAutomata.withGroups
            (\\maybe ->
                case maybe of
                    Just (Ant _ True) ->
                        "onAlive"
                    
                    Just (Ant _ False) ->
                        "onDead"
                    _ ->
                        ""
            )
```"""
