module Main exposing (..)

import Chapter.Editor
import Data.Editor
import ElmBook exposing (Book)
import ElmBook.Actions
import ElmBook.StatefulOptions
import ElmBook.ThemeOptions
import Task
import Time


main : Book Chapter.Editor.State
main =
    ElmBook.book "Elm-Cellautomata"
        |> ElmBook.withStatefulOptions
            [ ElmBook.StatefulOptions.initialState Chapter.Editor.init
            , ElmBook.StatefulOptions.subscriptions
                [ Time.every 500
                    (\_ ->
                        ElmBook.Actions.updateState
                            (\state ->
                                if state.editor.isRunning then
                                    state.editor
                                        |> Data.Editor.update state.automata
                                        |> (\editor -> { state | editor = editor })

                                else
                                    state
                            )
                    )
                ]
            ]
        |> ElmBook.withThemeOptions
            [ ElmBook.ThemeOptions.useHashBasedNavigation ]
        |> ElmBook.withChapters
            [ Chapter.Editor.chapter ]
