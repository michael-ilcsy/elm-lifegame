module Main exposing (..)

import Browser
import Html exposing (Html, div, input, label, table, tbody, td, text, tr)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput)
import Random
import SingleSlider exposing (SingleSlider)



---- MODEL ----


type alias Model =
    { cells : Cells
    , width : Int
    , height : Int
    , slider :
        { width : SingleSlider Msg
        , height : SingleSlider Msg
        , aliveProbability : SingleSlider Msg
        }
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
        width =
            10

        height =
            10

        aliveProbability =
            30

        widthSlider =
            SingleSlider.init
                { min = 1
                , max = 50
                , value = width
                , step = 1
                , onChange = ChangeWidth
                }

        heightSlider =
            SingleSlider.init
                { min = 1
                , max = 50
                , value = height
                , step = 1
                , onChange = ChangeHeight
                }

        aliveProbabilitySlider =
            SingleSlider.init
                { min = 0
                , max = 100
                , value = aliveProbability
                , step = 1
                , onChange = ChangeProbability
                }

        model =
            { cells = [ [] ]
            , width = 10
            , height = 10
            , slider =
                { width = widthSlider
                , height = heightSlider
                , aliveProbability = aliveProbabilitySlider
                }
            , aliveProbability = 50
            }
    in
    ( model
    , generateRandomCells model
    )



---- UPDATE ----


type Msg
    = ChangeWidth Float
    | ChangeHeight Float
    | ChangeProbability Float
    | GenerateRandomCells Cells


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeWidth width ->
            let
                newWidthSlider =
                    model.slider.width |> SingleSlider.update width

                oldSlider =
                    model.slider

                newSlider =
                    { oldSlider | width = newWidthSlider }

                newModel =
                    { model
                        | width =
                            width |> floor
                        , slider = newSlider
                    }
            in
            ( newModel
            , generateRandomCells newModel
            )

        ChangeHeight height ->
            let
                newWidthSlider =
                    model.slider.height |> SingleSlider.update height

                oldSlider =
                    model.slider

                newSlider =
                    { oldSlider | height = newWidthSlider }

                newModel =
                    { model
                        | height =
                            height |> floor
                        , slider = newSlider
                    }
            in
            ( newModel
            , generateRandomCells newModel
            )

        ChangeProbability probability ->
            let
                newWidthSlider =
                    model.slider.aliveProbability |> SingleSlider.update probability

                oldSlider =
                    model.slider

                newSlider =
                    { oldSlider | aliveProbability = newWidthSlider }

                newModel =
                    { model
                        | aliveProbability =
                            probability |> floor
                        , slider = newSlider
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
        [ div [] [ SingleSlider.view model.slider.width ]
        , div [] [ SingleSlider.view model.slider.height ]
        , div [] [ SingleSlider.view model.slider.aliveProbability ]
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
    td
        [ class
            (if cell == Alive then
                "alive"

             else
                ""
            )
        ]
        []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
