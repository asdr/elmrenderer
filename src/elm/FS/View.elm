module FS.View exposing (..)

import FS.Models exposing (Model)
import FS.Models.Application exposing (Application(Application))
import FS.Models.Base exposing (Route(..))
import FS.Messages exposing (Msg)
import FS.Players.Edit
import FS.Players.List
import FS.Core.Renderer
import Html exposing (Html, div, text)
import RemoteData


view : Model -> Html Msg
view model =
    let
        applicationProperties =
            case model.application of
                Application props ->
                    props
    in
        div []
            [ page model
            , div
                []
                [ text (toString applicationProperties) ]
            ]


page : Model -> Html Msg
page model =
    case model.route of
        DefaultRoute ->
            FS.Core.Renderer.render model.response

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
