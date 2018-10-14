module Main exposing (Model, Monster, MonsterViewModel, Msg(..), druido, hagreMetal, infinity, init, main, monster2ViewModel, monsterViewModel2View, ogarasu, samayouYoroi, slime, subscriptions, update, view, zoma)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



---- MODEL ----


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


infinity : Float
infinity =
    1 / 0



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


type alias Monster =
    { name : String, hp : Int, mp : Float, attack : Int, agility : Int }


slime =
    Monster "スライム" 8 0 9 4


ogarasu =
    Monster "おおがらす" 9 0 10 6


samayouYoroi =
    Monster "さまようよろい" 55 0 47 10


druido =
    Monster "ドルイド" 35 10 55 29


hagreMetal =
    Monster "はぐれメタル" 6 infinity 55 150


zoma =
    Monster "ゾーマ" 4700 infinity 360 80


type alias MonsterViewModel =
    { name : String, hp : String, mp : String, attack : String, agility : String }


monster2ViewModel : Monster -> MonsterViewModel
monster2ViewModel { name, hp, mp, attack, agility } =
    let
        hpText =
            String.fromInt hp

        mpText =
            String.fromFloat mp

        attackText =
            String.fromInt attack

        agilityText =
            String.fromInt agility
    in
    { name = name, hp = hpText, mp = mpText, attack = attackText, agility = agilityText }



---- VIEW ----


view : Model -> Html Msg
view model =
    table []
        [ thead []
            [ tr []
                [ th []
                    [ text "なまえ" ]
                , th [ class "" ]
                    [ text "HP"
                    , span [ class "arrow asc" ] []
                    ]
                , th [ class "" ]
                    [ text "MP"
                    , span [ class "arrow asc" ] []
                    ]
                , th [ class "" ]
                    [ text "こうげきりょく"
                    , span [ class "arrow asc" ] []
                    ]
                , th [ class "" ]
                    [ text "すばやさ"
                    , span [ class "arrow asc" ] []
                    ]
                ]
            ]
        , tbody [] <|
            List.map monsterViewModel2View
                [ MonsterViewModel "スライム" "8" "0" "9" "4"
                , MonsterViewModel "おおがらす" "9" "0" "10" "6"
                , MonsterViewModel "さまようよろい" "55" "0" "47" "10"
                , MonsterViewModel "ドルイド" "35" "10" "55" "29"
                , MonsterViewModel "はぐれメタル" "6" "∞" "55" "150"
                , MonsterViewModel "ゾーマ" "4700" "∞" "360" "80"
                ]
        ]


monsterViewModel2View : MonsterViewModel -> Html Msg
monsterViewModel2View { name, hp, mp, attack, agility } =
    tr []
        [ td [] [ text name ]
        , td [] [ text hp ]
        , td [] [ text mp ]
        , td [] [ text attack ]
        , td [] [ text agility ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
