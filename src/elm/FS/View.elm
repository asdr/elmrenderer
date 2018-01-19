module FS.View exposing (..)

import FS.Models exposing (Model)
import FS.Models.Core
import FS.Messages exposing (Msg)
import FS.Players.Edit
import FS.Players.List
import FS.Renderer
import Html exposing (Html, div, text)
import RemoteData


--import Xml
--import Xml.Query
--import Array
--import Dict
--import Xml.Encode


view : Model -> Html Msg
view model =
    div []
        [ page model
        ]


page : Model -> Html Msg
page model =
    case model.route of
        FS.Models.Core.DefaultRoute ->
            FS.Renderer.render model.response

        FS.Models.Core.PlayersRoute ->
            FS.Players.List.view model.players

        FS.Models.Core.PlayerRoute id ->
            playerEditPage model id

        FS.Models.Core.NotFoundRoute ->
            notFoundView


playerEditPage : Model -> String -> Html Msg
playerEditPage model playerId =
    case model.players of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "Loading ..."

        RemoteData.Success players ->
            let
                maybePlayer =
                    players
                        |> List.filter (\player -> player.id == playerId)
                        |> List.head
            in
                case maybePlayer of
                    Just player ->
                        FS.Players.Edit.view player

                    Nothing ->
                        notFoundView

        RemoteData.Failure err ->
            text (toString err)


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]
