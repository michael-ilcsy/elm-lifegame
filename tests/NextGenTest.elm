module NextGenTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Main exposing (Cell(..), nextGen)
import Random
import Random.List
import Shrink
import Test exposing (..)


all : Test
all =
    describe "次世代のセル計算のテスト"
        [ describe "周りに8マス存在するセルのテスト"
            [ describe "生きているセルのテスト"
                [ normalCellsTestHelper
                    "周りの生きたセルが0のときは死滅すること"
                    0
                    Dead
                , normalCellsTestHelper
                    "周りの生きたセルが1のときは死滅すること"
                    1
                    Dead
                , normalCellsTestHelper
                    "周りの生きたセルが2のときは生存すること"
                    2
                    Alive
                , normalCellsTestHelper
                    "周りの生きたセルが3のときは生存すること"
                    3
                    Alive
                , normalCellsTestHelper
                    "周りの生きたセルが4のときは死滅すること"
                    4
                    Dead
                , normalCellsTestHelper
                    "周りの生きたセルが5のときは死滅すること"
                    5
                    Dead
                , normalCellsTestHelper
                    "周りの生きたセルが6のときは死滅すること"
                    6
                    Dead
                , normalCellsTestHelper
                    "周りの生きたセルが7のときは死滅すること"
                    7
                    Dead
                , normalCellsTestHelper
                    "周りの生きたセルが8のときは死滅すること"
                    8
                    Dead
                ]
            ]
        ]


normalCellsTestHelper : String -> Int -> Cell -> Test
normalCellsTestHelper description aliveCellCount expectedCell =
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
                    (\list ->
                        let
                            baseCells =
                                [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]
                                    |> List.map
                                        (\cells ->
                                            List.map
                                                (\number ->
                                                    if List.member number (5 :: list) then
                                                        Alive

                                                    else
                                                        Dead
                                                )
                                                cells
                                        )

                            actualCell =
                                case nextGen baseCells of
                                    [ [ _, _, _ ], [ _, a, _ ], [ _, _, _ ] ] ->
                                        Just a

                                    _ ->
                                        Nothing
                        in
                        Expect.equal (Just expectedCell) actualCell
                    )
        in
        test


shuffledList : List Int -> Fuzzer (List Int)
shuffledList list =
    Fuzz.custom
        (Random.List.shuffle list)
        Shrink.noShrink
