module FS.Players.Edit exposing (..)

import FS.Models.Base exposing (Player)
import FS.Messages exposing (Msg)
import FS.Routing exposing (playersPath)
import Html exposing (..)
import Html.Attributes exposing (class, value, href)
import Html.Events exposing (onClick)


view : Player -> Html.Html Msg
view model =
    div []
        [ nav model
        , form model
        ]


nav : Player -> Html.Html Msg
nav model =
    div [ class "clearfix mb2 white bg-black p1" ]
        [ listBtn ]


form : Player -> Html.Html Msg
form player =
    div [ class "m3" ]
        [ h1 [] [ text player.name ]
        , formLevel player
        ]


formLevel : Player -> Html.Html Msg
formLevel player =
    div
        [ class "clearfix py1"
        ]
        [ div [ class "col col-5" ] [ text "Level" ]
        , div [ class "col col-7" ]
            [ span [ class "h2 bold" ] [ text (toString player.level) ]
            , btnLevelDecrease player
            , btnLevelIncrease player
            ]
        ]


btnLevelDecrease : Player -> Html.Html Msg
btnLevelDecrease player =
    let
        message =
            FS.Messages.ChangeLevel player -1
    in
        a [ class "btn ml1 h1", onClick message ]
            [ i [ class "fa fa-minus-circle" ] [] ]


btnLevelIncrease : Player -> Html.Html Msg
btnLevelIncrease player =
    let
        message =
            FS.Messages.ChangeLevel player 1
    in
        a [ class "btn ml1 h1", onClick message ]
            [ i [ class "fa fa-plus-circle" ] [] ]


listBtn : Html Msg
listBtn =
    a
        [ class "btn regular"
        , href playersPath
        ]
        [ i [ class "fa fa-chevron-left mr1" ] [], text "List" ]
