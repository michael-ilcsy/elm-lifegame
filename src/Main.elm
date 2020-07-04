module Main exposing (..)

import Browser
import Html exposing (Html, div, table, tbody, td, tr)
import Html.Attributes exposing (class)
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
            initSlider
                { min = 1
                , max = 50
                , value = width
                , step = 1
                , onChange = ChangeWidth
                }
                (\val _ ->
                    "横のマス数: " ++ (val |> String.fromFloat)
                )

        heightSlider =
            initSlider
                { min = 1
                , max = 50
                , value = height
                , step = 1
                , onChange = ChangeHeight
                }
                (\val _ ->
                    "縦のマス数: " ++ (val |> String.fromFloat)
                )

        aliveProbabilitySlider =
            initSlider
                { min = 0
                , max = 100
                , value = aliveProbability
                , step = 1
                , onChange = ChangeProbability
                }
                (\val _ ->
                    "生きているセルの確率: " ++ (val |> String.fromFloat) ++ "%"
                )

        model =
            { cells = [ [] ]
            , width = width
            , height = height
            , slider =
                { width = widthSlider
                , height = heightSlider
                , aliveProbability = aliveProbabilitySlider
                }
            , aliveProbability = aliveProbability
            }
    in
    ( model
    , generateRandomCells model
    )


initSlider :
    { min : Float
    , max : Float
    , step : Float
    , value : Float
    , onChange : Float -> msg
    }
    -> (Float -> Float -> String)
    -> SingleSlider msg
initSlider options formatter =
    SingleSlider.init
        options
        |> SingleSlider.withMaxFormatter (\_ -> "")
        |> SingleSlider.withMinFormatter (\_ -> "")
        |> SingleSlider.withValueFormatter formatter



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
        [ viewSlider model.slider.width
        , viewSlider model.slider.height
        , viewSlider model.slider.aliveProbability
        , table []
            [ tbody []
                [ tr [] (viewCells model.cells)
                ]
            ]
        ]


viewSlider : SingleSlider msg -> Html msg
viewSlider slider =
    div [] [ SingleSlider.view slider ]


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
