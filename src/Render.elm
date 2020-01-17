module Render exposing (render)

import Browser exposing (Document, document)
import CellAutomata exposing (Grid, Position)
import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Time


type alias Model state =
    Grid state


type Msg
    = Tick


width : Int
width =
    20


height : Int
height =
    width


update : (Grid state -> Position -> Maybe state -> Maybe state) -> Msg -> Model state -> ( Model state, Cmd Msg )
update step msg model =
    ( case msg of
        Tick ->
            List.range 0 width
                |> List.foldl
                    (\x m ->
                        List.range 0 height
                            |> List.foldl
                                (\y ->
                                    Dict.update
                                        ( x, y )
                                        (( x, y ) |> step model)
                                )
                                m
                    )
                    model
    , Cmd.none
    )


subscriptions : Model state -> Sub Msg
subscriptions _ =
    Time.every 200 (always Tick)


viewAutomata : (Maybe state -> Html msg) -> Model state -> List (Html msg)
viewAutomata renderState model =
    List.range 0 height
        |> List.map
            (\y ->
                Html.p [ Attributes.style "margin" "0" ]
                    (List.range 0 width
                        |> List.map
                            (\x ->
                                model
                                    |> Dict.get ( x, y )
                                    |> renderState
                            )
                    )
            )


view : String -> (Maybe state -> Html msg) -> Model state -> Document msg
view title renderState model =
    { title = title
    , body =
        model |> viewAutomata renderState
    }


none : Cmd Msg
none =
    Cmd.none


render :
    String
    -> (Grid state -> Position -> Maybe state -> Maybe state)
    -> (Maybe state -> Html Msg)
    -> Model state
    -> Program {} (Model state) Msg
render title step renderState init =
    document
        { init = \_ -> ( init, none )
        , view = view title renderState
        , update = update step
        , subscriptions = subscriptions
        }
