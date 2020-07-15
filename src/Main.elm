module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Random
import SingleSlider exposing (SingleSlider)
import Time



---- MODEL ----


type alias Model =
    { cells : Cells
    , width : Int
    , height : Int
    , sliders : Sliders
    , aliveProbability : Int
    , interval : Int
    , gameState : GameState
    }


type Cell
    = Alive
    | Dead


type alias Cells =
    Array (Array Cell)


type alias CellPosition =
    { x : Int, y : Int }


type alias Slider =
    SingleSlider Msg


type alias Sliders =
    { width : Slider
    , height : Slider
    , aliveProbability : Slider
    , interval : Slider
    }


type GameState
    = Setting
    | Running Int


init : ( Model, Cmd Msg )
init =
    let
        width =
            10

        height =
            10

        aliveProbability =
            30

        interval =
            100

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

        intervalSlider =
            initSlider
                { min = 50
                , max = 1000
                , value = interval
                , step = 50
                , onChange = ChangeInterval
                }
                (\val _ ->
                    "更新間隔: " ++ (val |> String.fromFloat) ++ "ms"
                )
    in
    ( { cells = generateEmptyCells { x = width, y = height }
      , width = width
      , height = height
      , sliders =
            { width = widthSlider
            , height = heightSlider
            , aliveProbability = aliveProbabilitySlider
            , interval = intervalSlider
            }
      , interval = interval
      , aliveProbability = aliveProbability
      , gameState = Setting
      }
    , Cmd.none
    )


generateEmptyCells : CellPosition -> Cells
generateEmptyCells { x, y } =
    Array.repeat y <| Array.repeat x Dead


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


isEmptyCells : Cells -> Bool
isEmptyCells cells =
    cells
        |> cellsToList
        |> List.all
            (List.all (\cell -> cell == Dead))


getCell : CellPosition -> Cells -> Cell
getCell { x, y } cells =
    Array.get y cells
        |> Maybe.andThen (Array.get x)
        |> Maybe.withDefault Dead


setCells : CellPosition -> Cell -> Cells -> Cells
setCells { x, y } cell cells =
    case Array.get y cells of
        Nothing ->
            cells

        Just cellRow ->
            Array.set y (Array.set x cell cellRow) cells



---- UPDATE ----


type Msg
    = ChangeWidth Float
    | ChangeHeight Float
    | ChangeProbability Float
    | ChangeInterval Float
    | Generate
    | GenerateRandomCells Cells
    | Clear
    | StartGame
    | StopGame
    | NextGen Int Time.Posix
    | ClickCell CellPosition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeWidth width ->
            case model.gameState of
                Setting ->
                    model
                        |> updateSliders
                            .width
                            (\slider sliders -> { sliders | width = slider })
                            (\value oldModel -> { oldModel | width = value })
                            width

                Running _ ->
                    ( model, Cmd.none )

        ChangeHeight height ->
            case model.gameState of
                Setting ->
                    model
                        |> updateSliders
                            .height
                            (\slider sliders -> { sliders | height = slider })
                            (\value oldModel -> { oldModel | height = value })
                            height

                Running _ ->
                    ( model, Cmd.none )

        ChangeProbability probability ->
            case model.gameState of
                Setting ->
                    model
                        |> updateSliders
                            .aliveProbability
                            (\slider sliders -> { sliders | aliveProbability = slider })
                            (\value oldModel -> { oldModel | aliveProbability = value })
                            probability

                Running _ ->
                    ( model, Cmd.none )

        ChangeInterval interval ->
            case model.gameState of
                Setting ->
                    model
                        |> updateSliders
                            .interval
                            (\slider sliders -> { sliders | interval = slider })
                            (\value oldModel -> { oldModel | interval = value })
                            interval

                Running _ ->
                    ( model, Cmd.none )

        Generate ->
            ( model, generateRandomCells model )

        GenerateRandomCells cells ->
            ( { model | cells = cells }, Cmd.none )

        Clear ->
            ( { model
                | cells = generateEmptyCells { x = model.width, y = model.height }
              }
            , Cmd.none
            )

        StartGame ->
            ( { model | gameState = Running 1 }, Cmd.none )

        StopGame ->
            ( { model | gameState = Setting }, Cmd.none )

        NextGen generationCount _ ->
            ( { model
                | gameState = Running generationCount
                , cells = nextGen model.cells
              }
            , Cmd.none
            )

        ClickCell position ->
            case model.gameState of
                Setting ->
                    let
                        toggledCell =
                            case getCell position model.cells of
                                Alive ->
                                    Dead

                                Dead ->
                                    Alive
                    in
                    ( { model
                        | cells =
                            setCells position toggledCell model.cells
                      }
                    , Cmd.none
                    )

                Running _ ->
                    ( model, Cmd.none )


randomCells : { a | width : Int, height : Int, aliveProbability : Int } -> Random.Generator Cells
randomCells { width, height, aliveProbability } =
    Random.list height
        (Random.list width
            (Random.weighted
                ( aliveProbability |> toFloat, Alive )
                [ ( 100 - aliveProbability |> toFloat, Dead ) ]
            )
        )
        |> Random.map listToCells


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
            model
                |> updater (newVal |> floor)
                |> slidersUpdater newSliders
                |> updateCellSize
    in
    ( newModel, Cmd.none )


nextGen : Cells -> Cells
nextGen cells =
    cells
        |> Array.indexedMap
            (\yIndex row ->
                Array.indexedMap
                    (\xIndex cell ->
                        resolveCell { x = xIndex, y = yIndex } cell cells
                    )
                    row
            )


resolveCell : CellPosition -> Cell -> Cells -> Cell
resolveCell { x, y } cell cells =
    let
        neighborsPositions =
            [ { x = x - 1, y = y - 1 }
            , { x = x - 1, y = y }
            , { x = x - 1, y = y + 1 }
            , { x = x, y = y - 1 }
            , { x = x, y = y + 1 }
            , { x = x + 1, y = y - 1 }
            , { x = x + 1, y = y }
            , { x = x + 1, y = y + 1 }
            ]

        numOfAliveNeighbors =
            neighborsPositions
                |> calcNumOfAliveNeighbors cells
    in
    case cell of
        Dead ->
            if numOfAliveNeighbors == 3 then
                Alive

            else
                Dead

        Alive ->
            if numOfAliveNeighbors <= 1 then
                Dead

            else if numOfAliveNeighbors == 2 || numOfAliveNeighbors == 3 then
                Alive

            else
                Dead


calcNumOfAliveNeighbors : Cells -> List CellPosition -> Int
calcNumOfAliveNeighbors cells neighborsPositions =
    neighborsPositions
        |> List.map
            (\position -> getCell position cells)
        |> List.foldl
            (\cell total ->
                case cell of
                    Alive ->
                        1 + total

                    Dead ->
                        0 + total
            )
            0


listToCells : List (List Cell) -> Cells
listToCells =
    Array.fromList >> Array.map Array.fromList


cellsToList : Cells -> List (List Cell)
cellsToList =
    Array.map Array.toList
        >> Array.toList


updateCellSize : Model -> Model
updateCellSize model =
    let
        newCells =
            generateEmptyCells { x = model.width, y = model.height }
                |> Array.indexedMap
                    (\yIndex row ->
                        Array.indexedMap
                            (\xIndex _ ->
                                getCell { x = xIndex, y = yIndex } model.cells
                            )
                            row
                    )
    in
    { model | cells = newCells }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Setting ->
            Sub.none

        Running generationCount ->
            Time.every (toFloat model.interval) <| NextGen <| generationCount + 1



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewSlider model.sliders.width
        , viewSlider model.sliders.height
        , viewSlider model.sliders.aliveProbability
        , viewSlider model.sliders.interval
        , div []
            [ viewGenerateButton model.gameState
            , viewClearButton model.gameState
            ]
        , viewStartButton model
        , viewStopButton model.gameState
        , viewGenerationCount model.gameState
        , table []
            [ tbody []
                [ tr [] (viewCells model.cells)
                ]
            ]
        ]


viewSlider : Slider -> Html Msg
viewSlider slider =
    div [] [ SingleSlider.view slider ]


viewGenerateButton : GameState -> Html Msg
viewGenerateButton gameState =
    button
        [ onClick Generate
        , disabled <| gameState /= Setting
        ]
        [ text "generate" ]


viewClearButton : GameState -> Html Msg
viewClearButton gameState =
    button
        [ onClick Clear
        , disabled <| gameState /= Setting
        ]
        [ text "clear" ]


viewStartButton : Model -> Html Msg
viewStartButton model =
    button
        [ onClick StartGame
        , disabled <| isEmptyCells model.cells || model.gameState /= Setting
        ]
        [ text "start" ]


viewStopButton : GameState -> Html Msg
viewStopButton gameState =
    button
        [ onClick StopGame
        , disabled <| gameState == Setting
        ]
        [ text "stop" ]


viewCells : Cells -> List (Html Msg)
viewCells cells =
    cells
        |> Array.indexedMap
            (\yIndex row ->
                tr [] <|
                    Array.toList <|
                        Array.indexedMap
                            (\xIndex cell ->
                                viewCell { x = xIndex, y = yIndex } cell
                            )
                            row
            )
        |> Array.toList


viewCell : CellPosition -> Cell -> Html Msg
viewCell position cell =
    td
        [ class
            (if cell == Alive then
                "alive"

             else
                ""
            )
        , onClick <| ClickCell position
        ]
        []


viewGenerationCount : GameState -> Html Msg
viewGenerationCount gameState =
    case gameState of
        Setting ->
            div [] []

        Running generationCount ->
            div [] [ text <| "Generation: " ++ String.fromInt generationCount ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
