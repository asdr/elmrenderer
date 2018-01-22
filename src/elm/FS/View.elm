module FS.View exposing (..)

import FS.Models exposing (Model)
import FS.Models.Base exposing (Route(..))
import FS.Messages exposing (Msg)
import FS.Players.Edit
import FS.Players.List
import FS.Core.Renderer
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import RemoteData


view : Model -> Html Msg
view model =
    render model


render : Model -> Html Msg
render model =
    FS.Core.Renderer.render model


page : Model -> Html Msg
page model =
    case model.route of
        DefaultRoute ->
            --FS.Core.Renderer.render model.response
            notFoundView

        PlayersRoute ->
            FS.Players.List.view model.players

        PlayerRoute id ->
            playerEditPage model id

        NotFoundRoute ->
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
