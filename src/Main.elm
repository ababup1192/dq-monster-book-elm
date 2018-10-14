module Main exposing
    ( HeaderViewModel
    , Model
    , Monster
    , MonsterViewModel
    , OrderBy(..)
    , OrderField(..)
    , SortCondition
    , changeSortCondition
    , createMonstersViewModel
    , infinity
    , monster2ViewModel
    , monsterViewModel2View
    , sortCondition2ViewModel
    , sortMonsters
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    { monsters : List Monster, maybeSortCondition : Maybe SortCondition }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { monsters =
            [ slime, ogarasu, samayouYoroi, druido, hagreMetal, zoma ]
      , maybeSortCondition = Nothing
      }
    , Cmd.none
    )


infinity : Float
infinity =
    1 / 0



---- UPDATE ----


type Msg
    = Sort OrderField


changeSortCondition : OrderField -> Maybe SortCondition -> Maybe SortCondition
changeSortCondition targetOrderField maybeSortCondition =
    case maybeSortCondition of
        Just { orderField, orderBy } ->
            if targetOrderField == orderField && orderBy == Dsc then
                Nothing

            else if targetOrderField /= orderField then
                Just <| SortCondition targetOrderField Asc

            else
                Just <| SortCondition targetOrderField Dsc

        Nothing ->
            Just <| SortCondition targetOrderField Asc


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ maybeSortCondition } as model) =
    case msg of
        Sort targetOrderField ->
            ( { model | maybeSortCondition = changeSortCondition targetOrderField maybeSortCondition }, Cmd.none )


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


ascComparison a b =
    compare a b


dscComparison a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


sortMonsters : SortCondition -> List Monster -> List Monster
sortMonsters { orderField, orderBy } monsters =
    let
        comparison f m1 m2 =
            case orderBy of
                Asc ->
                    ascComparison (f m1) (f m2)

                Dsc ->
                    dscComparison (f m1) (f m2)
    in
    case orderField of
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


defaultHeaderViewModel : HeaderViewModel
defaultHeaderViewModel =
    { hp = { active = "", arrow = "arrow asc" }
    , mp = { active = "", arrow = "arrow asc" }
    , attack = { active = "", arrow = "arrow asc" }
    , agility = { active = "", arrow = "arrow asc" }
    }


sortCondition2ViewModel : SortCondition -> HeaderViewModel
sortCondition2ViewModel { orderField, orderBy } =
    let
        orderBy2Arrow =
            case orderBy of
                Asc ->
                    "arrow asc"

                Dsc ->
                    "arrow dsc"
    in
    case orderField of
        Hp ->
            { defaultHeaderViewModel | hp = { active = "active", arrow = orderBy2Arrow } }

        Mp ->
            { defaultHeaderViewModel | mp = { active = "active", arrow = orderBy2Arrow } }

        Attack ->
            { defaultHeaderViewModel | attack = { active = "active", arrow = orderBy2Arrow } }

        Agility ->
            { defaultHeaderViewModel | agility = { active = "active", arrow = orderBy2Arrow } }



---- VIEW ----


createMonstersViewModel : List Monster -> Maybe SortCondition -> List MonsterViewModel
createMonstersViewModel monsters maybeSortCondition =
    case maybeSortCondition of
        Just sortCondition ->
            monsters |> sortMonsters sortCondition |> List.map monster2ViewModel

        Nothing ->
            monsters |> List.map monster2ViewModel


view : Model -> Html Msg
view { monsters, maybeSortCondition } =
    let
        headerView =
            case maybeSortCondition of
                Just sortCondition ->
                    sortCondition |> sortCondition2ViewModel |> headerViewModel2View

                Nothing ->
                    defaultHeaderViewModel |> headerViewModel2View

        monstersView =
            maybeSortCondition |> createMonstersViewModel monsters |> List.map monsterViewModel2View
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
