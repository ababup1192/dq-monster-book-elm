module Main exposing
    ( By(..)
    , Dir(..)
    , HeaderViewModel
    , Model
    , Monster
    , MonsterViewModel
    , Order(..)
    , changeOrder
    , createMonstersViewModel
    , infinity
    , monster2ViewModel
    , monsterViewModel2View
    , order2ViewModel
    , sortMonsters
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    { monsters : List Monster, order : Order }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { monsters =
            [ slime, ogarasu, samayouYoroi, druido, hagreMetal, zoma ]
      , order = Default
      }
    , Cmd.none
    )


infinity : Float
infinity =
    1 / 0



---- UPDATE ----


type Msg
    = Sort By


changeOrder : By -> Order -> Order
changeOrder targetBy currentOrder =
    case currentOrder of
        Order by dir ->
            if by == targetBy && dir == Dsc then
                Default

            else if by /= targetBy then
                Order targetBy Asc

            else
                Order targetBy Dsc

        Default ->
            Order targetBy Asc


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ order } as model) =
    case msg of
        Sort targetBy ->
            ( { model | order = changeOrder targetBy order }, Cmd.none )


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


ascComparison : comparable -> comparable -> Basics.Order
ascComparison a b =
    compare a b


dscComparison : comparable -> comparable -> Basics.Order
dscComparison a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


sortMonsters : Order -> List Monster -> List Monster
sortMonsters currentOrder monsters =
    case currentOrder of
        Default ->
            monsters

        Order by dir ->
            let
                comparison f m1 m2 =
                    case dir of
                        Asc ->
                            ascComparison (f m1) (f m2)

                        Dsc ->
                            dscComparison (f m1) (f m2)
            in
            case by of
                Hp ->
                    List.sortWith (comparison .hp) monsters

                Mp ->
                    List.sortWith (comparison .mp) monsters

                Attack ->
                    List.sortWith (comparison .attack) monsters

                Agility ->
                    List.sortWith (comparison .agility) monsters


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


type By
    = Hp
    | Mp
    | Attack
    | Agility


type Dir
    = Asc
    | Dsc


type Order
    = Default
    | Order By Dir


type alias HeaderFieldViewModel =
    { active : String, arrow : String }


type alias HeaderViewModel =
    { hp : HeaderFieldViewModel, mp : HeaderFieldViewModel, attack : HeaderFieldViewModel, agility : HeaderFieldViewModel }


defaultHeaderViewModel : HeaderViewModel
defaultHeaderViewModel =
    { hp = { active = "", arrow = "arrow asc" }
    , mp = { active = "", arrow = "arrow asc" }
    , attack = { active = "", arrow = "arrow asc" }
    , agility = { active = "", arrow = "arrow asc" }
    }


order2ViewModel : Order -> HeaderViewModel
order2ViewModel order =
    case order of
        Default ->
            defaultHeaderViewModel

        Order by dir ->
            let
                orderBy2Arrow =
                    case dir of
                        Asc ->
                            "arrow asc"

                        Dsc ->
                            "arrow dsc"
            in
            case by of
                Hp ->
                    { defaultHeaderViewModel | hp = { active = "active", arrow = orderBy2Arrow } }

                Mp ->
                    { defaultHeaderViewModel | mp = { active = "active", arrow = orderBy2Arrow } }

                Attack ->
                    { defaultHeaderViewModel | attack = { active = "active", arrow = orderBy2Arrow } }

                Agility ->
                    { defaultHeaderViewModel | agility = { active = "active", arrow = orderBy2Arrow } }



---- VIEW ----


createMonstersViewModel : List Monster -> Order -> List MonsterViewModel
createMonstersViewModel monsters order =
    monsters |> sortMonsters order |> List.map monster2ViewModel


view : Model -> Html Msg
view { monsters, order } =
    let
        headerView =
            order2ViewModel order |> headerViewModel2View

        monstersView =
            createMonstersViewModel monsters order |> List.map monsterViewModel2View
    in
    table []
        [ headerView
        , tbody [] monstersView
        ]


headerViewModel2View : HeaderViewModel -> Html Msg
headerViewModel2View { hp, mp, attack, agility } =
    thead []
        [ tr []
            [ th []
                [ text "なまえ" ]
            , th [ class hp.active, onClick <| Sort Hp ]
                [ text "HP"
                , span [ class hp.arrow ] []
                ]
            , th [ class mp.active, onClick <| Sort Mp ]
                [ text "MP"
                , span [ class mp.arrow ] []
                ]
            , th [ class attack.active, onClick <| Sort Attack ]
                [ text "こうげきりょく"
                , span [ class attack.arrow ] []
                ]
            , th [ class agility.active, onClick <| Sort Agility ]
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
