module Main exposing (..)

import Chapter.GameOfLife
import Chapter.LangtonsAnt
import Data.Editor exposing (Editor)
import ElmBook exposing (Book, Msg)
import ElmBook.Actions
import ElmBook.StatefulOptions
import ElmBook.ThemeOptions
import Time
import W.Styles


type alias Model =
    { gameOfLife : Editor Chapter.GameOfLife.State
    , langtonsAnt : Editor Chapter.LangtonsAnt.State
    }


init : Model
init =
    { gameOfLife = Chapter.GameOfLife.init
    , langtonsAnt = Chapter.LangtonsAnt.init
    }


everyTick : Msg Model
everyTick =
    ElmBook.Actions.updateState
        (\model ->
            { gameOfLife = Data.Editor.update model.gameOfLife
            , langtonsAnt = Data.Editor.update model.langtonsAnt
            }
        )


main : Book Model
main =
    ElmBook.book "Elm-Cellautomata"
        |> ElmBook.withStatefulOptions
            [ ElmBook.StatefulOptions.initialState init
            , ElmBook.StatefulOptions.subscriptions
                [ Time.every 500 (\_ -> everyTick)
                ]
            ]
        |> ElmBook.withThemeOptions
            [ ElmBook.ThemeOptions.useHashBasedNavigation
            , ElmBook.ThemeOptions.globals
                [ W.Styles.globalStyles
                , W.Styles.baseTheme
                ]
            ]
        |> ElmBook.withChapters
            [ Chapter.GameOfLife.chapter
                { read = .gameOfLife
                , writeTo = \model editor -> { model | gameOfLife = editor }
                }
            , Chapter.LangtonsAnt.chapter
                { read = .langtonsAnt
                , writeTo = \model editor -> { model | langtonsAnt = editor }
                }
            ]
