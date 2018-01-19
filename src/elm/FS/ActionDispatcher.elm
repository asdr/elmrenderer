module FS.ActionDispatcher exposing (..)

import Debug
import Dict exposing (Dict)
import Http
import Xml
import Xml.Encode
import Xml.Query
import FS.Messages exposing (Msg)
import FS.Models exposing (Model)
import FS.Models.Http exposing (Response)


dispatch : Model -> Result Http.Error Response -> ( Model, Cmd Msg )
dispatch model response =
    case response of
        Result.Ok res ->
            let
                actionList =
                    Xml.Query.tags "action" (Maybe.withDefault Xml.Encode.null res.xml)

                sessionId =
                    Dict.get "SessionId" res.headers
            in
                consumeActionList actionList { model | sessionId = sessionId }

        Result.Err _ ->
            ( model, Cmd.none )


consumeActionList : List Xml.Value -> Model -> ( Model, Cmd Msg )
consumeActionList actionList model =
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
                dispatchActionNode model action
                    |> consumeActionList rest


dispatchActionNode : Model -> Xml.Value -> Model
dispatchActionNode model actionNode =
    case actionNode of
        Xml.Tag name attributes children ->
            let
                objectTypeResult =
                    Xml.Query.string (Maybe.withDefault (Xml.StrNode "") (Dict.get "objectType" attributes))

                methodResult =
                    Xml.Query.string (Maybe.withDefault (Xml.StrNode "") (Dict.get "method" attributes))
            in
                case objectTypeResult of
                    Ok objectType ->
                        case methodResult of
                            Ok method ->
                                Dict.remove "objectType" attributes
                                    |> Dict.remove "method"
                                    |> dispatchAction model objectType method

                            Err _ ->
                                model

                    Err _ ->
                        model

        _ ->
            model


dispatchAction : Model -> String -> String -> Dict String a -> Model
dispatchAction model objectType method attributes =
    Debug.log (objectType ++ "." ++ method ++ " - " ++ (toString attributes)) model
