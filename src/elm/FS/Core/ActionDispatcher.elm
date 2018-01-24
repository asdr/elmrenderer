module FS.Core.ActionDispatcher exposing (..)

import Dict exposing (Dict)
import Http
import Xml
import Xml.Encode exposing (null)
import Xml.Query exposing (string, tags)
import FS.Messages exposing (Msg)
import FS.Models exposing (Model)
import FS.Models.Actions exposing (Action(Action), ApplicationAction(ApplicationOpen), MainFrameAction(MainFrameInitialize, MainFrameSetVisible))
import FS.Models.Http exposing (Response)
import FS.Update.Application as ApplicationUpdate
import FS.Update.MainFrame as MainFrameUpdate


dispatch : Model -> Result Http.Error Response -> ( Model, Cmd Msg )
dispatch model response =
    case response of
        Result.Ok res ->
            let
                xml =
                    (Maybe.withDefault null res.xml)

                actionList =
                    tags "action" xml

                sessionId =
                    Dict.get "SessionId" res.headers
            in
                consumeActionList xml actionList { model | sessionId = sessionId }

        Result.Err _ ->
            ( model, Cmd.none )


consumeActionList : Xml.Value -> List Xml.Value -> Model -> ( Model, Cmd Msg )
consumeActionList response actionList model =
    let
        head =
            List.head actionList

        rest =
            Maybe.withDefault [] (List.tail actionList)
    in
        case head of
            Nothing ->
                ( model, Cmd.none )

            Just action ->
                dispatchActionNode response model action
                    |> consumeActionList response rest


dispatchActionNode : Xml.Value -> Model -> Xml.Value -> Model
dispatchActionNode response model actionNode =
    case actionNode of
        Xml.Tag name attributes children ->
            let
                objectTypeResult =
                    string (Maybe.withDefault (Xml.StrNode "") (Dict.get "objectType" attributes))

                methodResult =
                    string (Maybe.withDefault (Xml.StrNode "") (Dict.get "method" attributes))
            in
                case objectTypeResult of
                    Ok objectType ->
                        case methodResult of
                            Ok method ->
                                attributes
                                    |> Dict.remove "objectType"
                                    |> Dict.remove "method"
                                    |> dispatchAction response model objectType method
                                    |> Debug.log
                                        ("ActionDispatcher.dispatchActionNode - "
                                            ++ method
                                        )

                            Err _ ->
                                model

                    Err _ ->
                        model

        _ ->
            model


dispatchAction : Xml.Value -> Model -> String -> String -> Dict String Xml.Value -> Model
dispatchAction response model objectType method attributes =
    let
        maybeOidValue =
            Dict.get "oid" attributes

        maybeObjectId =
            case maybeOidValue of
                Nothing ->
                    Nothing

                Just oidValue ->
                    oidValue
                        |> Xml.Query.int
                        |> Result.toMaybe
    in
        case objectType of
            "Application" ->
                case method of
                    "Open" ->
                        ApplicationUpdate.update response model (Action (ApplicationOpen maybeObjectId))

                    _ ->
                        model

            "MainFrame" ->
                case method of
                    "Initialize" ->
                        MainFrameUpdate.update response model (Action (MainFrameInitialize maybeObjectId))

                    "SetVisible" ->
                        MainFrameUpdate.update response model (Action (MainFrameSetVisible maybeObjectId (Just False)))
                            |> Debug.log "ActionDispatcher.dispatchAction - SetVisible"

                    _ ->
                        model

            _ ->
                model
