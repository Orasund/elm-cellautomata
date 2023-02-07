module View.Editor exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout


display :
    { offset : ( Int, Int )
    , width : Int
    , height : Int
    , onClick : ( Int, Int ) -> Maybe msg
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
                                        maybeString
                                            |> Maybe.withDefault ""
                                            |> Html.text
                                            |> Layout.buttonEl
                                                { onPress = args.onClick ( x, y )
                                                , label =
                                                    maybeString
                                                        |> Maybe.withDefault "Empty"
                                                }
                                                (Layout.centered
                                                    ++ [ Attr.style "width" "24px"
                                                       , Attr.style "height" "24px"
                                                       ]
                                                )
                                   )
                        )
                    |> Layout.row []
            )
        |> Layout.column []
