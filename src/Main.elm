module Main exposing (..)

import Browser
import Html exposing (Html, div, input, label, table, tbody, td, text, tr)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { cells : Cells
    , width : Int
    , height : Int
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
      , width = 10
      , height = 10
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = InputWidth String
    | InputHeight String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputWidth width ->
            ( { model
                | width =
                    width
                        |> String.toInt
                        |> Maybe.withDefault model.width
              }
            , Cmd.none
            )

        InputHeight height ->
            ( { model
                | height =
                    height
                        |> String.toInt
                        |> Maybe.withDefault model.height
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ label [] [ text "縦", input [ type_ "text", value <| String.fromInt model.height, onInput InputHeight ] [] ]
        , label [] [ text "横", input [ type_ "text", value <| String.fromInt model.width, onInput InputWidth ] [] ]
        , table []
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
