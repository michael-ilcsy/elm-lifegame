module Main exposing (..)

import Browser
import Html exposing (Html, div, input, label, table, tbody, td, text, tr)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Random



---- MODEL ----


type alias Model =
    { cells : Cells
    , width : Int
    , height : Int
    , aliveProbability : Int
    }


type Cell
    = Alive
    | Dead


type alias Cells =
    List (List Cell)


init : ( Model, Cmd Msg )
init =
    let
        model =
            { cells = [ [] ]
            , width = 10
            , height = 10
            , aliveProbability = 50
            }
    in
    ( model
    , generateRandomCells model
    )



---- UPDATE ----


type Msg
    = InputWidth String
    | InputHeight String
    | GenerateRandomCells Cells


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputWidth width ->
            let
                newModel =
                    { model
                        | width =
                            width
                                |> String.toInt
                                |> Maybe.withDefault model.width
                    }
            in
            ( newModel
            , generateRandomCells newModel
            )

        InputHeight height ->
            let
                newModel =
                    { model
                        | height =
                            height
                                |> String.toInt
                                |> Maybe.withDefault model.height
                    }
            in
            ( newModel
            , generateRandomCells newModel
            )

        GenerateRandomCells cells ->
            ( { model | cells = cells }, Cmd.none )


randomCells : { a | width : Int, height : Int, aliveProbability : Int } -> Random.Generator Cells
randomCells { width, height, aliveProbability } =
    Random.list height
        (Random.list width
            (Random.weighted
                ( aliveProbability |> toFloat, Alive )
                [ ( 100 - aliveProbability |> toFloat, Dead ) ]
            )
        )


generateRandomCells : { a | width : Int, height : Int, aliveProbability : Int } -> Cmd Msg
generateRandomCells model =
    Random.generate GenerateRandomCells (randomCells model)



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
