module FS.Update exposing (..)

import FS.ActionDispatcher
import FS.Commands exposing (savePlayerCmd)
import FS.Models exposing (Model)
import FS.Models.Core exposing (Player)
import FS.Messages exposing (Msg)
import FS.Routing exposing (parseLocation)
import RemoteData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FS.Messages.OnFetchPlayers response ->
            ( { model | players = response }, Cmd.none )

        FS.Messages.OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        FS.Messages.ChangeLevel player howMuch ->
            let
                updatedPlayer =
                    { player | level = player.level + howMuch }
            in
                ( model, savePlayerCmd updatedPlayer )

        FS.Messages.OnPlayerSave (Ok player) ->
            ( updatePlayer model player, Cmd.none )

        FS.Messages.OnPlayerSave (Err error) ->
            ( model, Cmd.none )

        FS.Messages.OnFetchResponse response ->
            FS.ActionDispatcher.dispatch model response


updatePlayer : Model -> Player -> Model
updatePlayer model updatedPlayer =
    let
        pick currentPlayer =
            if updatedPlayer.id == currentPlayer.id then
                updatedPlayer
            else
                currentPlayer

        updatePlayerList players =
            List.map pick players

        updatedPlayers =
            RemoteData.map updatePlayerList model.players
    in
        { model | players = updatedPlayers }
