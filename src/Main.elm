module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



---- MODEL ----


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



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
        , tbody []
            [ tr []
                [ td [] [ text "スライム" ]
                , td [] [ text "8" ]
                , td [] [ text "0" ]
                , td [] [ text "9" ]
                , td [] [ text "4" ]
                ]
            , tr []
                [ td [] [ text "おおがらす" ]
                , td [] [ text "9" ]
                , td [] [ text "0" ]
                , td [] [ text "10" ]
                , td [] [ text "6" ]
                ]
            , tr []
                [ td [] [ text "さまようよろい" ]
                , td [] [ text "55" ]
                , td [] [ text "0" ]
                , td [] [ text "47" ]
                , td [] [ text "10" ]
                ]
            , tr []
                [ td [] [ text "ドルイド" ]
                , td [] [ text "35" ]
                , td [] [ text "10" ]
                , td [] [ text "55" ]
                , td [] [ text "29" ]
                ]
            , tr []
                [ td [] [ text "はぐれメタル" ]
                , td [] [ text "6" ]
                , td [] [ text "∞" ]
                , td [] [ text "55" ]
                , td [] [ text "150" ]
                ]
            , tr []
                [ td [] [ text "ゾーマ" ]
                , td [] [ text "4700" ]
                , td [] [ text "∞" ]
                , td [] [ text "360" ]
                , td [] [ text "80" ]
                ]
            ]
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
