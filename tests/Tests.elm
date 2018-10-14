module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
    exposing
        ( Monster
        , MonsterViewModel
        , OrderBy(..)
        , OrderField(..)
        , SortCondition
        , infinity
        , monster2ViewModel
        , sortCondition2ViewModel
        )
import Test exposing (..)


type alias TestCaseName =
    String


monster2ViewModelTest : TestCaseName -> Monster -> MonsterViewModel -> Test
monster2ViewModelTest testCaseName monster monsterViewModel =
    test testCaseName <|
        \_ ->
            let
                actual =
                    monster2ViewModel <| monster

                expected =
                    monsterViewModel
            in
            Expect.equal actual expected


suite : Test
suite =
    describe "The Main module"
        [ describe "monster2ViewModel"
            [ monster2ViewModelTest "スライム は ViewModel になった！" (Monster "スライム" 8 0 9 4) (MonsterViewModel "スライム" "8" "0" "9" "4")
            , monster2ViewModelTest "はぐれメタル は ViewModel になった！(無限に注意!)" (Monster "はぐれメタル" 6 infinity 55 150) (MonsterViewModel "はぐれメタル" "6" "∞" "55" "150")
            , monster2ViewModelTest "ゾーマ は ViewModel になった！(無限に注意！)" (Monster "ゾーマ" 4700 infinity 360 80) (MonsterViewModel "ゾーマ" "4700" "∞" "360" "80")
            ]
        , describe "sortCondition2ViewModel"
            [ test "Hp Dsc " <|
                \_ ->
                    let
                        actual =
                            sortCondition2ViewModel <| SortCondition Hp Dsc

                        expected =
                            { hp = { active = "active", arrow = "arrow dsc" }
                            , mp = { active = "", arrow = "arrow asc" }
                            , attack = { active = "", arrow = "arrow asc" }
                            , agility = { active = "", arrow = "arrow asc" }
                            }
                    in
                    Expect.equal actual expected
            , test "Mp Asc " <|
                \_ ->
                    let
                        actual =
                            sortCondition2ViewModel <| SortCondition Mp Asc

                        expected =
                            { hp = { active = "", arrow = "arrow asc" }
                            , mp = { active = "active", arrow = "arrow asc" }
                            , attack = { active = "", arrow = "arrow asc" }
                            , agility = { active = "", arrow = "arrow asc" }
                            }
                    in
                    Expect.equal actual expected
            ]
        ]
