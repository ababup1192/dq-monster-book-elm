module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Monster, MonsterViewModel, monster2ViewModel)
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
            ]
        ]
