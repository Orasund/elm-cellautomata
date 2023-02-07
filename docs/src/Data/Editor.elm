module Data.Editor exposing (..)

import CellAutomata.Advanced as CellAutomata exposing (Automata)
import Dict exposing (Dict)


type alias Editor state =
    { dict : Dict ( Int, Int ) state
    , width : Int
    , height : Int
    , isRunning : Bool
    }


init :
    { width : Int
    , height : Int
    }
    -> Dict ( Int, Int ) state
    -> Editor state
init args dict =
    { dict = dict
    , height = args.height
    , width = args.width
    , isRunning = False
    }


toggleRunning : Editor state -> Editor state
toggleRunning editor =
    { editor | isRunning = not editor.isRunning }


update : Automata ( Int, Int ) state -> Editor state -> Editor state
update automata editor =
    List.range 0 (editor.height - 1)
        |> List.foldl
            (\y dict ->
                List.range 0 (editor.width - 1)
                    |> List.foldl
                        (\x d ->
                            d
                                |> Dict.update ( x, y )
                                    (CellAutomata.step automata editor.dict ( x, y ))
                        )
                        dict
            )
            editor.dict
        |> (\dict -> { editor | dict = dict })
