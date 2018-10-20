module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
    exposing
        ( By(..)
        , Dir(..)
        , HeaderViewModel
        , Monster
        , MonsterViewModel
        , Order(..)
        , changeOrder
        , createMonstersViewModel
        , infinity
        , monster2ViewModel
        , order2ViewModel
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


order2ViewModelTest : TestCaseName -> Order -> HeaderViewModel -> Test
order2ViewModelTest testCaseName order headerViewModel =
    test testCaseName <|
        \_ ->
            let
                actual =
                    order2ViewModel order

                expected =
                    headerViewModel
            in
            Expect.equal actual expected


monstersSortTest : TestCaseName -> Order -> List Monster -> List Monster -> Test
monstersSortTest testCaseName order monsters sortedMonsters =
    test testCaseName <|
        \_ ->
            let
                actual =
                    sortMonsters order monsters

                expected =
                    sortedMonsters
            in
            Expect.equal actual expected


changeOrderTest : TestCaseName -> By -> Order -> Order -> Test
changeOrderTest testCaseName by order changedOrder =
    test testCaseName <|
        \_ ->
            let
                actual =
                    changeOrder by order

                expected =
                    changedOrder
            in
            Expect.equal actual expected


suite : Test
suite =
    describe "The Main module"
        [ describe "monster2ViewModel"
            [ monster2ViewModelTest
                "スライム は ViewModel になった！"
                (Monster "スライム" 8 0 9 4)
                (MonsterViewModel "スライム" "8" "0" "9" "4")
            , monster2ViewModelTest
                "はぐれメタル は ViewModel になった！(無限に注意!)"
                (Monster "はぐれメタル" 6 infinity 55 150)
                (MonsterViewModel "はぐれメタル" "6" "∞" "55" "150")
            , monster2ViewModelTest
                "ゾーマ は ViewModel になった！(無限に注意！)"
                (Monster "ゾーマ" 4700 infinity 360 80)
                (MonsterViewModel "ゾーマ" "4700" "∞" "360" "80")
            ]
        , describe "order2ViewModel"
            [ order2ViewModelTest "Hp Dsc"
                (Order Hp Dsc)
                { hp = { active = "active", arrow = "arrow dsc" }
                , mp = { active = "", arrow = "arrow asc" }
                , attack = { active = "", arrow = "arrow asc" }
                , agility = { active = "", arrow = "arrow asc" }
                }
            , order2ViewModelTest "Mp Asc"
                (Order Mp Asc)
                { hp = { active = "", arrow = "arrow asc" }
                , mp = { active = "active", arrow = "arrow asc" }
                , attack = { active = "", arrow = "arrow asc" }
                , agility = { active = "", arrow = "arrow asc" }
                }
            , order2ViewModelTest "Attack Dsc"
                (Order Attack Dsc)
                { hp = { active = "", arrow = "arrow asc" }
                , mp = { active = "", arrow = "arrow asc" }
                , attack = { active = "active", arrow = "arrow dsc" }
                , agility = { active = "", arrow = "arrow asc" }
                }
            ]
        , describe "sortMonsters"
            [ monstersSortTest "HPで昇順ソート！"
                (Order Hp Asc)
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
                (Order Hp Dsc)
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
                (Order Mp Dsc)
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
            , changeOrderTest
                "初期状態からHPのソートボタンを押すと、HPが昇順ソートになる"
                Hp
                Default
                (Order Hp Asc)
            , changeOrderTest
                "HPが昇順ソートされている状態でHPのソートボタンを押すと、HPが降順ソートになる"
                Hp
                (Order Hp Asc)
                (Order Hp Dsc)
            , changeOrderTest
                "HPが降順ソートされている状態でHPのソートボタンを押すと、初期状態になる"
                Hp
                (Order Hp Dsc)
                Default
            , changeOrderTest
                "HPが昇順ソートされている状態でMPのソートボタンを押すと、MPの昇順ソートになる"
                Mp
                (Order Hp Dsc)
                (Order Mp Asc)
            , test "デフォルトの並びのモンスターデータが、HPソートボタンを押されたことにより、HP昇順ソートの並びの図鑑になる" <|
                \_ ->
                    let
                        defaultMonsters =
                            [ Monster "スライム" 8 0 9 4
                            , Monster "おおがらす" 9 0 10 6
                            , Monster "さまようよろい" 55 0 47 10
                            , Monster "ドルイド" 35 10 55 29
                            , Monster "はぐれメタル" 6 infinity 55 150
                            , Monster "ゾーマ" 4700 infinity 360 80
                            ]

                        actual =
                            Default |> changeOrder Hp |> createMonstersViewModel defaultMonsters

                        expected =
                            [ MonsterViewModel "はぐれメタル" "6" "∞" "55" "150"
                            , MonsterViewModel "スライム" "8" "0" "9" "4"
                            , MonsterViewModel "おおがらす" "9" "0" "10" "6"
                            , MonsterViewModel "ドルイド" "35" "10" "55" "29"
                            , MonsterViewModel "さまようよろい" "55" "0" "47" "10"
                            , MonsterViewModel "ゾーマ" "4700" "∞" "360" "80"
                            ]
                    in
                    Expect.equal actual expected
            , test "「HPの昇順ソート」がされている状態で、HPソートボタンを押されたことにより、「HP降順ソート」の並びの図鑑になる" <|
                \_ ->
                    let
                        defaultMonsters =
                            [ Monster "スライム" 8 0 9 4
                            , Monster "おおがらす" 9 0 10 6
                            , Monster "さまようよろい" 55 0 47 10
                            , Monster "ドルイド" 35 10 55 29
                            , Monster "はぐれメタル" 6 infinity 55 150
                            , Monster "ゾーマ" 4700 infinity 360 80
                            ]

                        actual =
                            Order Hp Asc |> changeOrder Hp |> createMonstersViewModel defaultMonsters

                        expected =
                            [ MonsterViewModel "ゾーマ" "4700" "∞" "360" "80"
                            , MonsterViewModel "さまようよろい" "55" "0" "47" "10"
                            , MonsterViewModel "ドルイド" "35" "10" "55" "29"
                            , MonsterViewModel "おおがらす" "9" "0" "10" "6"
                            , MonsterViewModel "スライム" "8" "0" "9" "4"
                            , MonsterViewModel "はぐれメタル" "6" "∞" "55" "150"
                            ]
                    in
                    Expect.equal actual expected
            ]
        ]
