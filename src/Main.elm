module Main exposing
    ( HeaderViewModel
    , Model
    , Monster
    , MonsterViewModel
    , OrderBy(..)
    , OrderField(..)
    , SortCondition
    , infinity
    , monster2ViewModel
    , monsterViewModel2View
    , sortCondition2ViewModel
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



---- MODEL ----


type alias Model =
    { monsters : List Monster }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { monsters =
            [ slime, ogarasu, samayouYoroi, druido, hagreMetal, zoma ]
      }
    , Cmd.none
    )


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
            if mp == infinity then
                "∞"

            else
                String.fromFloat mp

        attackText =
            String.fromInt attack

        agilityText =
            String.fromInt agility
    in
    { name = name, hp = hpText, mp = mpText, attack = attackText, agility = agilityText }


type OrderField
    = Hp
    | Mp
    | Attack
    | Agility


type OrderBy
    = Asc
    | Dsc


type alias SortCondition =
    { orderField : OrderField, orderBy : OrderBy }


type alias HeaderFieldViewModel =
    { active : String, arrow : String }


type alias HeaderViewModel =
    { hp : HeaderFieldViewModel, mp : HeaderFieldViewModel, attack : HeaderFieldViewModel, agility : HeaderFieldViewModel }


sortCondition2ViewModel : SortCondition -> HeaderViewModel
sortCondition2ViewModel { orderField, orderBy } =
    let
        defaultViewModel =
            { hp = { active = "", arrow = "arrow asc" }
            , mp = { active = "", arrow = "arrow asc" }
            , attack = { active = "", arrow = "arrow asc" }
            , agility = { active = "", arrow = "arrow asc" }
            }

        orderBy2Arrow =
            case orderBy of
                Asc ->
                    "arrow asc"

                Dsc ->
                    "arrow dsc"
    in
    case orderField of
        Hp ->
            { defaultViewModel | hp = { active = "active", arrow = orderBy2Arrow } }

        Mp ->
            { defaultViewModel | mp = { active = "active", arrow = orderBy2Arrow } }

        Attack ->
            { defaultViewModel | attack = { active = "active", arrow = orderBy2Arrow } }

        Agility ->
            { defaultViewModel | agility = { active = "active", arrow = orderBy2Arrow } }



---- VIEW ----


view : Model -> Html Msg
view { monsters } =
    let
        monstersView =
            monsters |> List.map monster2ViewModel |> List.map monsterViewModel2View
    in
    table []
        [ headerViewModel2View
            { hp = { active = "active", arrow = "arrow dsc" }
            , mp = { active = "", arrow = "arrow asc" }
            , attack = { active = "", arrow = "arrow asc" }
            , agility = { active = "", arrow = "arrow asc" }
            }
        , tbody [] monstersView
        ]


headerViewModel2View : HeaderViewModel -> Html Msg
headerViewModel2View { hp, mp, attack, agility } =
    thead []
        [ tr []
            [ th []
                [ text "なまえ" ]
            , th [ class hp.active ]
                [ text "HP"
                , span [ class hp.arrow ] []
                ]
            , th [ class mp.active ]
                [ text "MP"
                , span [ class mp.arrow ] []
                ]
            , th [ class attack.active ]
                [ text "こうげきりょく"
                , span [ class attack.arrow ] []
                ]
            , th [ class agility.active ]
                [ text "すばやさ"
                , span [ class agility.arrow ] []
                ]
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
