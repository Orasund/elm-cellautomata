module Render exposing (main)

import CellAutomata.LifeLike as Automata exposing (Grid,Location,AliveNeighbors(..),Automata,State(..))
import Dict exposing (Dict)
import Time
import Html
import Html.Attributes as Attributes
import Browser exposing (Document,document)

type alias Model =
    Grid

type Msg = 
    Tick

width: Int
width = 20

height: Int
height = width

init : flags -> (Model,Cmd Msg)
init _ =
    ( Dict.empty
        |> Dict.insert (0,1) Alive
        |> Dict.insert (1,2) Alive
        |> Dict.insert (2,0) Alive
        |> Dict.insert (2,1) Alive
        |> Dict.insert (2,2) Alive
    , Cmd.none
    )
    

automata : Automata
automata =
    [ {from = Just Alive, neighbors = TwoAlive, to = Just Alive}
    , {from = Just Alive, neighbors = ThreeAlive, to = Just Alive}
    , {from = Just Alive, neighbors = AnyAmount, to = Nothing}
    , {from = Nothing, neighbors = ThreeAlive, to = Just Alive}
    ]
    |>Automata.automata

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        Tick ->
            List.range 0 width
            |> List.foldl
                (\x m ->
                    List.range 0 height
                    |> List.foldl
                        (\y -> 
                            Dict.update
                                (x,y)
                                ((x,y) |> Automata.step automata model)
                        )
                        m
                )
                model
    , Cmd.none
    )
    

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 200 (always Tick)


view : Model -> Document Msg
view model =
    { title = "Game of Life"
    , body =
        List.range 0 height
        |> List.map 
            (\y ->
                Html.p [ Attributes.style "margin" "0"]
                    ( List.range 0 width
                        |> List.map
                            (\x ->
                                case model |> Dict.get (x,y) of
                                    Nothing -> Html.text "🔲"
                                    Just Alive -> Html.text "🔴"
                            )
                    )
            )
    }


main : Program {} Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }