module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Monster, MonsterViewModel, infinity, monster2ViewModel)
import Test exposing (..)


suite : Test
suite =
    describe "The Main module"
        [ describe "monster2ViewModel"
            -- Nest as many descriptions as you like.
            [ test "スライム は ViewModel になった！" <|
                \_ ->
                    let
                        actual =
                            monster2ViewModel <| Monster "スライム" 8 0 9 4

                        expected =
                            MonsterViewModel "スライム" "8" "0" "9" "4"
                    in
                    Expect.equal actual expected
            , test "はぐれメタル は ViewModel になった！" <|
                \_ ->
                    let
                        actual =
                            monster2ViewModel <| Monster "はぐれメタル" 6 infinity 55 150

                        expected =
                            MonsterViewModel "はぐれメタル" "6" "∞" "55" "150"
                    in
                    Expect.equal actual expected
            , test "ゾーマ は ViewModel になった！" <|
                \_ ->
                    let
                        actual =
                            monster2ViewModel <| Monster "ゾーマ" 4700 infinity 360 80

                        expected =
                            MonsterViewModel "ゾーマ" "4700" "∞" "360" "80"
                    in
                    Expect.equal actual expected
            ]
        ]
