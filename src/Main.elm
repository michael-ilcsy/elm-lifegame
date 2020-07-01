module Main exposing (..)

import Browser
import Html exposing (Html, div, table, tbody, td, tr)



---- MODEL ----


type alias Model =
    { cells : Cells
    }


type Cell
    = Alive
    | Dead


type alias Cells =
    List (List Cell)


init : ( Model, Cmd Msg )
init =
    ( { cells =
            [ [ Dead
              , Dead
              ]
            , [ Alive
              , Alive
              ]
            ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ table []
            [ tbody []
                [ tr [] (viewCells model.cells)
                ]
            ]
        ]


viewCells : Cells -> List (Html msg)
viewCells cells =
    cells
        |> List.map viewCellRow


viewCellRow : List Cell -> Html msg
viewCellRow cells =
    tr []
        (cells
            |> List.map viewCell
        )


viewCell : Cell -> Html msg
viewCell cell =
    td [] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
