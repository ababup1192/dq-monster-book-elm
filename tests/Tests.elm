module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
    exposing
        ( HeaderViewModel
        , Monster
        , MonsterViewModel
        , OrderBy(..)
        , OrderField(..)
        , SortCondition
        , changeSortCondition
        , infinity
        , monster2ViewModel
        , sortCondition2ViewModel
        , sortMonsters
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


sortCondition2ViewModelTest : TestCaseName -> SortCondition -> HeaderViewModel -> Test
sortCondition2ViewModelTest testCaseName sortCondition headerViewModel =
    test testCaseName <|
        \_ ->
            let
                actual =
                    sortCondition2ViewModel sortCondition

                expected =
                    headerViewModel
            in
            Expect.equal actual expected


monstersSortTest : TestCaseName -> SortCondition -> List Monster -> List Monster -> Test
monstersSortTest testCaseName sortCondition monsters sortedMonsters =
    test testCaseName <|
        \_ ->
            let
                actual =
                    sortMonsters sortCondition monsters

                expected =
                    sortedMonsters
            in
            Expect.equal actual expected


changeSortConditionTest : TestCaseName -> OrderField -> Maybe SortCondition -> Maybe SortCondition -> Test
changeSortConditionTest testCaseName orderField maybeSortCondition maybeChangedSortCondition =
    test testCaseName <|
        \_ ->
            let
                actual =
                    changeSortCondition orderField maybeSortCondition

                expected =
                    maybeChangedSortCondition
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
            [ sortCondition2ViewModelTest "Hp Dsc"
                (SortCondition Hp Dsc)
                { hp = { active = "active", arrow = "arrow dsc" }
                , mp = { active = "", arrow = "arrow asc" }
                , attack = { active = "", arrow = "arrow asc" }
                , agility = { active = "", arrow = "arrow asc" }
                }
            , sortCondition2ViewModelTest "Mp Asc"
                (SortCondition Mp Asc)
                { hp = { active = "", arrow = "arrow asc" }
                , mp = { active = "active", arrow = "arrow asc" }
                , attack = { active = "", arrow = "arrow asc" }
                , agility = { active = "", arrow = "arrow asc" }
                }
            , sortCondition2ViewModelTest "Attack Dsc"
                (SortCondition Attack Dsc)
                { hp = { active = "", arrow = "arrow asc" }
                , mp = { active = "", arrow = "arrow asc" }
                , attack = { active = "active", arrow = "arrow dsc" }
                , agility = { active = "", arrow = "arrow asc" }
                }
            ]
        , describe "sortMonsters"
            [ monstersSortTest "HPで昇順ソート！"
                (SortCondition Hp Asc)
                [ Monster "スライム" 8 0 9 4
                , Monster "おおがらす" 9 0 10 6
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ドルイド" 35 10 55 29
                , Monster "はぐれメタル" 6 infinity 55 150
                , Monster "ゾーマ" 4700 infinity 360 80
                ]
                [ Monster "はぐれメタル" 6 infinity 55 150
                , Monster "スライム" 8 0 9 4
                , Monster "おおがらす" 9 0 10 6
                , Monster "ドルイド" 35 10 55 29
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ゾーマ" 4700 infinity 360 80
                ]
            , monstersSortTest "HPで降順ソート！"
                (SortCondition Hp Dsc)
                [ Monster "スライム" 8 0 9 4
                , Monster "おおがらす" 9 0 10 6
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ドルイド" 35 10 55 29
                , Monster "はぐれメタル" 6 infinity 55 150
                , Monster "ゾーマ" 4700 infinity 360 80
                ]
                [ Monster "ゾーマ" 4700 infinity 360 80
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ドルイド" 35 10 55 29
                , Monster "おおがらす" 9 0 10 6
                , Monster "スライム" 8 0 9 4
                , Monster "はぐれメタル" 6 infinity 55 150
                ]
            , monstersSortTest "MPで降順ソート！"
                (SortCondition Mp Dsc)
                [ Monster "スライム" 8 0 9 4
                , Monster "おおがらす" 9 0 10 6
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ドルイド" 35 10 55 29
                , Monster "はぐれメタル" 6 infinity 55 150
                , Monster "ゾーマ" 4700 infinity 360 80
                ]
                [ Monster "はぐれメタル" 6 infinity 55 150
                , Monster "ゾーマ" 4700 infinity 360 80
                , Monster "ドルイド" 35 10 55 29
                , Monster "スライム" 8 0 9 4
                , Monster "おおがらす" 9 0 10 6
                , Monster "さまようよろい" 55 0 47 10
                ]
            , changeSortConditionTest
                "初期状態からHPのソートボタンを押すと、HPが昇順ソートになる"
                Hp
                Nothing
                (Just <| SortCondition Hp Asc)
            , changeSortConditionTest
                "HPが昇順ソートされている状態でHPのソートボタンを押すと、HPが降順ソートになる"
                Hp
                (Just <| SortCondition Hp Asc)
                (Just <| SortCondition Hp Dsc)
            , changeSortConditionTest
                "HPが降順ソートされている状態でHPのソートボタンを押すと、初期状態になる"
                Hp
                (Just <| SortCondition Hp Dsc)
                Nothing
            , changeSortConditionTest
                "HPが昇順ソートされている状態でMPのソートボタンを押すと、MPの昇順ソートになる"
                Mp
                (Just <| SortCondition Hp Dsc)
                (Just <| SortCondition Mp Asc)
            ]
        ]
