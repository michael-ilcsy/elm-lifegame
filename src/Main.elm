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
    in
    ( { cells = generateEmptyCells { width = width, height = height }
      , width = width
      , height = height
      , sliders =
            initSliders
                { width = width
                , height = height
                , aliveProbability = aliveProbability
                , interval = interval
                }
      , interval = interval
      , aliveProbability = aliveProbability
      , gameState = Setting
      }
    , Cmd.none
    )


generateEmptyCells : { width : Int, height : Int } -> Cells
generateEmptyCells { width, height } =
    Array.repeat height <| Array.repeat width Dead


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
    SingleSlider.init options
        |> SingleSlider.withMaxFormatter (always "")
        |> SingleSlider.withMinFormatter (always "")
        |> SingleSlider.withValueFormatter formatter


initSliders : { width : Float, height : Float, aliveProbability : Float, interval : Float } -> Sliders
initSliders { width, height, aliveProbability, interval } =
    let
        widthSlider =
            initSlider
                { min = 1
                , max = 50
                , value = width
                , step = 1
                , onChange = ChangeWidth
                }
                (\val _ -> "横のマス数: " ++ String.fromFloat val)

        heightSlider =
            initSlider
                { min = 1
                , max = 50
                , value = height
                , step = 1
                , onChange = ChangeHeight
                }
                (\val _ -> "縦のマス数: " ++ String.fromFloat val)

        aliveProbabilitySlider =
            initSlider
                { min = 0
                , max = 100
                , value = aliveProbability
                , step = 1
                , onChange = ChangeProbability
                }
                (\val _ -> "生きているセルの確率: " ++ String.fromFloat val ++ "%")

        intervalSlider =
            initSlider
                { min = 50
                , max = 1000
                , value = interval
                , step = 50
                , onChange = ChangeInterval
                }
                (\val _ -> "更新間隔: " ++ String.fromFloat val ++ "ms")
    in
    { width = widthSlider
    , height = heightSlider
    , aliveProbability = aliveProbabilitySlider
    , interval = intervalSlider
    }


isEmptyCells : Cells -> Bool
isEmptyCells cells =
    cellsToList cells
        |> List.all (List.all ((==) Dead))


getCell : CellPosition -> Cells -> Maybe Cell
getCell { x, y } cells =
    Array.get y cells
        |> Maybe.andThen (Array.get x)


setCell : CellPosition -> Cell -> Cells -> Cells
setCell { x, y } cell cells =
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
    | GenerateRandomCells
    | GetRandomCells Cells
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
                    updateSliders
                        .width
                        (\slider sliders -> { sliders | width = slider })
                        (\value oldModel -> { oldModel | width = value })
                        width
                        model

                Running _ ->
                    ( model, Cmd.none )

        ChangeHeight height ->
            case model.gameState of
                Setting ->
                    updateSliders
                        .height
                        (\slider sliders -> { sliders | height = slider })
                        (\value oldModel -> { oldModel | height = value })
                        height
                        model

                Running _ ->
                    ( model, Cmd.none )

        ChangeProbability probability ->
            case model.gameState of
                Setting ->
                    updateSliders
                        .aliveProbability
                        (\slider sliders -> { sliders | aliveProbability = slider })
                        (\value oldModel -> { oldModel | aliveProbability = value })
                        probability
                        model

                Running _ ->
                    ( model, Cmd.none )

        ChangeInterval interval ->
            case model.gameState of
                Setting ->
                    updateSliders
                        .interval
                        (\slider sliders -> { sliders | interval = slider })
                        (\value oldModel -> { oldModel | interval = value })
                        interval
                        model

                Running _ ->
                    ( model, Cmd.none )

        GenerateRandomCells ->
            ( model, generateRandomCells model )

        GetRandomCells cells ->
            ( { model | cells = cells }, Cmd.none )

        Clear ->
            ( { model
                | cells = generateEmptyCells { width = model.width, height = model.height }
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
                            case getCell position model.cells |> Maybe.withDefault Dead of
                                Alive ->
                                    Dead

                                Dead ->
                                    Alive
                    in
                    ( { model
                        | cells =
                            setCell position toggledCell model.cells
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
                ( toFloat aliveProbability, Alive )
                [ ( toFloat <| 100 - aliveProbability, Dead ) ]
            )
        )
        |> Random.map listToCells


generateRandomCells : { a | width : Int, height : Int, aliveProbability : Int } -> Cmd Msg
generateRandomCells model =
    Random.generate GetRandomCells (randomCells model)


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
            sliderUpdater newSlider model.sliders

        slidersUpdater sliders oldModel =
            { oldModel | sliders = sliders }

        newModel =
            updater (floor newVal) model
                |> slidersUpdater newSliders
                |> updateCellSize
    in
    ( newModel, Cmd.none )


nextGen : Cells -> Cells
nextGen cells =
    Array.indexedMap
        (\yIndex row ->
            Array.indexedMap
                (\xIndex cell ->
                    resolveCell { x = xIndex, y = yIndex } cell cells
                )
                row
        )
        cells


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
            calcNumOfAliveNeighbors cells neighborsPositions
    in
    case ( cell, numOfAliveNeighbors ) of
        ( Dead, 3 ) ->
            Alive

        ( Dead, _ ) ->
            Dead

        ( Alive, 2 ) ->
            Alive

        ( Alive, 3 ) ->
            Alive

        ( Alive, _ ) ->
            Dead


calcNumOfAliveNeighbors : Cells -> List CellPosition -> Int
calcNumOfAliveNeighbors cells neighborsPositions =
    List.map
        (\position -> getCell position cells |> Maybe.withDefault Dead)
        neighborsPositions
        |> List.map cellToInt
        |> List.sum


cellToInt : Cell -> Int
cellToInt cell =
    case cell of
        Alive ->
            1

        Dead ->
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
            generateEmptyCells { width = model.width, height = model.height }
                |> Array.indexedMap
                    (\yIndex row ->
                        Array.indexedMap
                            (\xIndex _ ->
                                getCell { x = xIndex, y = yIndex } model.cells |> Maybe.withDefault Dead
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
    div [ class "content" ]
        [ div [ class "slider-area" ]
            [ viewSlider model.sliders.width
            , viewSlider model.sliders.height
            ]
        , div [ class "slider-area" ]
            [ viewSlider model.sliders.aliveProbability
            , viewSlider model.sliders.interval
            ]
        , div [ class "button-area" ]
            [ div []
                [ viewGenerateButton model.gameState
                , viewClearButton model.gameState
                ]
            , viewStartButton model
            , viewStopButton model.gameState
            ]
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
        [ onClick GenerateRandomCells
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
    Array.indexedMap
        (\yIndex row ->
            tr [] <|
                Array.toList <|
                    Array.indexedMap
                        (\xIndex cell ->
                            viewCell { x = xIndex, y = yIndex } cell
                        )
                        row
        )
        cells
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
            div [ class "generation" ] []

        Running generationCount ->
            div [ class "generation" ] [ text <| "Generation: " ++ String.fromInt generationCount ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
