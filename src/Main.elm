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
    , sliders : Sliders
    , aliveProbability : Int
    }


type Cell
    = Alive
    | Dead


type alias Cells =
    List (List Cell)


type alias Slider =
    SingleSlider Msg


type alias Sliders =
    { width : Slider
    , height : Slider
    , aliveProbability : Slider
    }


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
            , sliders =
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
            model
                |> updateSliders
                    .width
                    (\slider sliders -> { sliders | width = slider })
                    (\value oldModel -> { oldModel | width = value })
                    width

        ChangeHeight height ->
            model
                |> updateSliders
                    .height
                    (\slider sliders -> { sliders | height = slider })
                    (\value oldModel -> { oldModel | height = value })
                    height

        ChangeProbability probability ->
            model
                |> updateSliders
                    .aliveProbability
                    (\slider sliders -> { sliders | aliveProbability = slider })
                    (\value oldModel -> { oldModel | aliveProbability = value })
                    probability

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


updateSliders :
    (Sliders -> Slider)
    -> (Slider -> Sliders -> Sliders)
    -> (Int -> Model -> Model)
    -> Float
    -> Model
    -> ( Model, Cmd Msg )
updateSliders slider sliderUpdater updater newVal model =
    let
        newSlider =
            model.sliders |> slider |> SingleSlider.update newVal

        newSliders =
            model.sliders |> sliderUpdater newSlider

        slidersUpdater =
            \sliders oldModel -> { oldModel | sliders = sliders }

        newModel =
            model |> updater (newVal |> floor) |> slidersUpdater newSliders
    in
    ( newModel, generateRandomCells newModel )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewSlider model.sliders.width
        , viewSlider model.sliders.height
        , viewSlider model.sliders.aliveProbability
        , table []
            [ tbody []
                [ tr [] (viewCells model.cells)
                ]
            ]
        ]


viewSlider : Slider -> Html Msg
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
