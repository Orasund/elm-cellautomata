module Data.Editor exposing (..)

import CellAutomata.Advanced as CellAutomata exposing (Automata)
import Dict exposing (Dict)


type alias Editor state =
    { dict : Dict ( Int, Int ) state
    , init : Dict ( Int, Int ) state
    , selected : state
    , width : Int
    , height : Int
    , isRunning : Bool
    , automata : Automata ( Int, Int ) state
    }


init :
    { width : Int
    , height : Int
    , automata : Automata ( Int, Int ) state
    , selected : state
    }
    -> Dict ( Int, Int ) state
    -> Editor state
init args dict =
    { dict = dict
    , init = dict
    , height = args.height
    , width = args.width
    , isRunning = False
    , automata = args.automata
    , selected = args.selected
    }


reset : Editor state -> Editor state
reset editor =
    { editor | dict = editor.init }


toggleRunning : Editor state -> Editor state
toggleRunning editor =
    { editor | isRunning = not editor.isRunning }


updateAt : ( Int, Int ) -> (Maybe state -> Maybe state) -> Editor state -> Editor state
updateAt pos fun editor =
    { editor | dict = editor.dict |> Dict.update pos fun }


insert : ( Int, Int ) -> Editor state -> Editor state
insert pos editor =
    { editor | dict = editor.dict |> Dict.insert pos editor.selected }


removeAt : ( Int, Int ) -> Editor state -> Editor state
removeAt pos editor =
    { editor | dict = editor.dict |> Dict.remove pos }


select : state -> Editor state -> Editor state
select state editor =
    { editor | selected = state }


update : Editor state -> Editor state
update editor =
    if editor.isRunning then
        List.range 0 (editor.height - 1)
            |> List.foldl
                (\y dict ->
                    List.range 0 (editor.width - 1)
                        |> List.foldl
                            (\x d ->
                                d
                                    |> Dict.update ( x, y )
                                        (CellAutomata.step editor.automata editor.dict ( x, y ))
                            )
                            dict
                )
                editor.dict
            |> (\dict -> { editor | dict = dict })

    else
        editor
