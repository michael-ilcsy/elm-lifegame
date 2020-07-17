module NextGenTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Main exposing (Cell(..), Cells, cellsToList, listToCells, nextGen)
import Random.List
import Shrink
import Test exposing (..)


all : Test
all =
    describe "次世代のセル計算のテスト"
        [ describe "周りに8マス存在するセルのテスト"
            [ describe "生きているセルのテスト"
                ([ ( 0, Dead )
                 , ( 1, Dead )
                 , ( 2, Alive )
                 , ( 3, Alive )
                 , ( 4, Dead )
                 , ( 5, Dead )
                 , ( 6, Dead )
                 , ( 7, Dead )
                 , ( 8, Dead )
                 ]
                    |> List.map (\( count, cell ) -> aliveNormalCellsTestHelper count cell)
                )
            , describe "死んでいるセルのテスト"
                ([ ( 0, Dead )
                 , ( 1, Dead )
                 , ( 2, Dead )
                 , ( 3, Alive )
                 , ( 4, Dead )
                 , ( 5, Dead )
                 , ( 6, Dead )
                 , ( 7, Dead )
                 , ( 8, Dead )
                 ]
                    |> List.map (\( count, cell ) -> deadNormalCellsTestHelper count cell)
                )
            ]
        , describe "周りに8マスしないセルのテスト"
            [ test "左上のセルのテスト" <|
                specialCellsTestHelper
                    [ [ Alive, Dead ], [ Dead, Dead ] ]
                    [ [ Dead, Dead ], [ Dead, Dead ] ]
            , test "右上のセルのテスト" <|
                specialCellsTestHelper
                    [ [ Dead, Alive ], [ Dead, Dead ] ]
                    [ [ Dead, Dead ], [ Dead, Dead ] ]
            , test "左下のセルのテスト" <|
                specialCellsTestHelper
                    [ [ Dead, Dead ], [ Alive, Dead ] ]
                    [ [ Dead, Dead ], [ Dead, Dead ] ]
            , test "右下のセルのテスト" <|
                specialCellsTestHelper
                    [ [ Dead, Dead ], [ Dead, Alive ] ]
                    [ [ Dead, Dead ], [ Dead, Dead ] ]
            , test "左辺のセルのテスト" <|
                specialCellsTestHelper
                    [ [ Dead, Dead ], [ Alive, Dead ], [ Dead, Dead ] ]
                    [ [ Dead, Dead ], [ Dead, Dead ], [ Dead, Dead ] ]
            , test "上辺のセルのテスト" <|
                specialCellsTestHelper
                    [ [ Dead, Alive, Dead ], [ Dead, Dead, Dead ] ]
                    [ [ Dead, Dead, Dead ], [ Dead, Dead, Dead ] ]
            , test "右辺のセルのテスト" <|
                specialCellsTestHelper
                    [ [ Dead, Dead ], [ Dead, Alive ], [ Dead, Dead ] ]
                    [ [ Dead, Dead ], [ Dead, Dead ], [ Dead, Dead ] ]
            , test "下辺のセルのテスト" <|
                specialCellsTestHelper
                    [ [ Dead, Dead, Dead ], [ Dead, Alive, Dead ] ]
                    [ [ Dead, Dead, Dead ], [ Dead, Dead, Dead ] ]
            ]
        ]


aliveNormalCellsTestHelper : Int -> Cell -> Test
aliveNormalCellsTestHelper =
    normalCellsTestHelper True


deadNormalCellsTestHelper : Int -> Cell -> Test
deadNormalCellsTestHelper =
    normalCellsTestHelper False


cellsTestDescription : Int -> Cell -> String
cellsTestDescription count cell =
    String.concat
        [ "周りの生きたセルが"
        , String.fromInt count
        , "のときは"
        , case cell of
            Alive ->
                "生存"

            Dead ->
                "死滅"
        , "すること"
        ]


normalCellsTestHelper : Bool -> Int -> Cell -> Test
normalCellsTestHelper isTargetAlive aliveCellCount expectedCell =
    let
        description =
            cellsTestDescription aliveCellCount expectedCell
    in
    if aliveCellCount >= 9 then
        test description <|
            \_ ->
                Expect.fail "周りのセルの数を9以上には出来ません"

    else
        let
            aliveCellsFuzzer =
                shuffledList [ 1, 2, 3, 4, 6, 7, 8, 9 ]
                    |> Fuzz.map (List.take aliveCellCount)

            test =
                fuzz aliveCellsFuzzer
                    description
                    (cellsExpectation isTargetAlive expectedCell)
        in
        test


shuffledList : List Int -> Fuzzer (List Int)
shuffledList list =
    Fuzz.custom
        (Random.List.shuffle list)
        (Shrink.list Shrink.int)


cellsExpectation : Bool -> Cell -> List Int -> Expectation
cellsExpectation isTargetAlive expectedCell list =
    let
        aliveCellList =
            if isTargetAlive then
                5 :: list

            else
                list

        baseCells =
            [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]
                |> List.map
                    (\cells ->
                        List.map
                            (\number ->
                                if List.member number aliveCellList then
                                    Alive

                                else
                                    Dead
                            )
                            cells
                    )

        actualCell =
            case cellsToList <| nextGen <| listToCells baseCells of
                [ [ _, _, _ ], [ _, a, _ ], [ _, _, _ ] ] ->
                    Just a

                _ ->
                    Nothing
    in
    Expect.equal (Just expectedCell) actualCell


specialCellsTestHelper : List (List Cell) -> List (List Cell) -> (() -> Expectation)
specialCellsTestHelper previousCells expectedCells =
    \_ ->
        Expect.equal
            (nextGen <|
                listToCells previousCells
            )
        <|
            listToCells expectedCells
