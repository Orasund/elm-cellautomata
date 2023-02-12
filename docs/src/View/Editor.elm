module View.Editor exposing (..)

import Data.Editor exposing (Editor)
import Dict exposing (Dict)
import ElmBook exposing (Msg)
import ElmBook.Actions
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import W.Button
import W.ButtonGroup


toHtml :
    { read : model -> Editor state
    , writeTo : model -> Editor state -> model
    , toString : state -> String
    , placableStates : List state
    }
    -> model
    -> Html (Msg model)
toHtml args state =
    let
        editor =
            args.read state

        toggleRunning =
            ElmBook.Actions.updateState
                (\s ->
                    s
                        |> args.read
                        |> Data.Editor.toggleRunning
                        |> args.writeTo s
                )

        insertAt =
            \pos ->
                ElmBook.Actions.updateState
                    (\s ->
                        s
                            |> args.read
                            |> Data.Editor.insert pos
                            |> args.writeTo s
                    )

        deleteAt =
            \pos ->
                ElmBook.Actions.updateState
                    (\s ->
                        s
                            |> args.read
                            |> Data.Editor.removeAt pos
                            |> args.writeTo s
                    )

        select =
            \selectedState ->
                ElmBook.Actions.updateState
                    (\s ->
                        s
                            |> args.read
                            |> Data.Editor.select selectedState
                            |> args.writeTo s
                    )
    in
    [ display
        { offset = ( 0, 0 )
        , width = editor.width
        , height = editor.height
        , onClick =
            \pos ->
                if Dict.member pos editor.dict then
                    deleteAt pos

                else
                    insertAt pos
        }
        (editor.dict
            |> Dict.map (\_ -> args.toString)
        )
    , [ editor
            |> controls
                { onClick = toggleRunning
                }
      , args.placableStates
            |> stateSelection
                { onClick = select
                , toString = args.toString
                , selected = editor.selected
                }
      ]
        |> Layout.row [ Layout.spacing 8 ]
    ]
        |> Layout.column [ Layout.spacing 16, Layout.alignAtCenter ]


stateSelection : { onClick : state -> msg, toString : state -> String, selected : state } -> List state -> Html msg
stateSelection args list =
    W.ButtonGroup.view
        [ W.ButtonGroup.highlighted ((==) args.selected)
        , W.ButtonGroup.rounded
        ]
        { items = list
        , toLabel =
            \state ->
                state
                    |> args.toString
                    |> Html.text
                    |> List.singleton
        , onClick = args.onClick
        }


controls : { onClick : msg } -> Editor state -> Html msg
controls args editor =
    W.Button.view
        (if editor.isRunning then
            []

         else
            [ W.Button.primary ]
        )
        { label =
            if editor.isRunning then
                [ Html.text "Stop" ]

            else
                [ Html.text "Run" ]
        , onClick = args.onClick
        }
        |> Layout.el []


display :
    { offset : ( Int, Int )
    , width : Int
    , height : Int
    , onClick : ( Int, Int ) -> msg
    }
    -> Dict ( Int, Int ) String
    -> Html msg
display args dict =
    let
        ( offsetX, offsetY ) =
            args.offset
    in
    List.range offsetY (offsetY + args.height - 1)
        |> List.map
            (\y ->
                List.range offsetX (offsetX + args.width - 1)
                    |> List.map
                        (\x ->
                            dict
                                |> Dict.get ( x, y )
                                |> (\maybeString ->
                                        W.Button.view
                                            [ W.Button.htmlAttrs
                                                (Layout.centered
                                                    ++ [ Attr.style "width" "32px"
                                                       , Attr.style "height" "32px"
                                                       ]
                                                )
                                            , W.Button.small
                                            , W.Button.primary
                                            , W.Button.outlined
                                            ]
                                            { label =
                                                [ maybeString
                                                    |> Maybe.withDefault ""
                                                    |> Html.text
                                                ]
                                            , onClick = args.onClick ( x, y )
                                            }
                                   )
                        )
                    |> Layout.row []
            )
        |> Layout.column []
