module FS.Update.MainFrame exposing (update)

import Dict exposing (Dict)
import Xml
import Xml.Query exposing (tags)
import FS.Core.Types exposing (CommonProperties, MainFrameProperties, FormspiderType(MainFrame), findObject, mainFrame)
import FS.Models exposing (Model, updateBubble)
import FS.Models.Actions exposing (Action(Action), MainFrameAction(MainFrameInitialize))
import FS.Update.Panel as PanelUpdate


update : Xml.Value -> Model -> Action MainFrameAction -> Model
update response model action =
    case action of
        Action appAction ->
            case appAction of
                MainFrameInitialize mfId ->
                    let
                        maybeMainFrame =
                            Debug.log "maybeMainFrame" (findObject (Debug.log "mainFrameId" mfId) model.child)
                    in
                        case maybeMainFrame of
                            Nothing ->
                                initialize response mfId
                                    |> updateBubble model

                            Just mainFrame ->
                                case mainFrame of
                                    MainFrame props ->
                                        MainFrame { props | id = 100 }
                                            |> Just
                                            |> updateBubble model

                                    _ ->
                                        model


initialize : Xml.Value -> Maybe Int -> Maybe FormspiderType
initialize response maybeMainFrameId =
    case maybeMainFrameId of
        Nothing ->
            Nothing

        Just mainFrameId ->
            let
                maybeMainFrameTag =
                    tags "mainFrame" response
                        |> List.filter
                            (\x ->
                                case x of
                                    Xml.Tag n attrs children ->
                                        let
                                            maybeId =
                                                attrs
                                                    |> Dict.get ("oid")
                                                    |> Maybe.withDefault (Xml.IntNode -1)
                                                    |> Xml.Query.int
                                                    |> Result.toMaybe
                                        in
                                            case maybeId of
                                                Nothing ->
                                                    False

                                                Just id ->
                                                    if id == mainFrameId then
                                                        True
                                                    else
                                                        False

                                    _ ->
                                        False
                            )
                        |> List.head
            in
                initializeMainFrame response maybeMainFrameTag


initializeMainFrame : Xml.Value -> Maybe Xml.Value -> Maybe FormspiderType
initializeMainFrame response maybeMainFrameTag =
    case maybeMainFrameTag of
        Nothing ->
            Nothing

        Just mainFrameTag ->
            case mainFrameTag of
                Xml.Tag name attributes children ->
                    let
                        maybePanelTag : Maybe Xml.Value
                        maybePanelTag =
                            tags "panel" children
                                |> List.head
                    in
                        case maybePanelTag of
                            Just panelTag ->
                                case panelTag of
                                    Xml.Tag pname pattributes pchildren ->
                                        let
                                            maybePanelId =
                                                pattributes
                                                    |> Dict.get ("oid")
                                                    |> Maybe.withDefault (Xml.IntNode -1)
                                                    |> Xml.Query.int
                                                    |> Result.toMaybe
                                        in
                                            case maybePanelId of
                                                Nothing ->
                                                    initializeObject attributes Nothing

                                                Just panelId ->
                                                    PanelUpdate.initialize response maybePanelId
                                                        |> initializeObject attributes

                                    _ ->
                                        initializeObject attributes Nothing

                            Nothing ->
                                initializeObject attributes Nothing

                _ ->
                    Nothing


initializeObject : Dict String Xml.Value -> Maybe FormspiderType -> Maybe FormspiderType
initializeObject attributes child =
    let
        maybeId =
            attributes
                |> Dict.get ("oid")
                |> Maybe.withDefault (Xml.IntNode -1)
                |> Xml.Query.int
                |> Result.toMaybe

        maybeName =
            attributes
                |> Dict.get ("name")
                |> Maybe.withDefault (Xml.StrNode "")
                |> Xml.Query.string
                |> Result.toMaybe
    in
        case maybeId of
            Nothing ->
                Nothing

            Just id ->
                case maybeName of
                    Nothing ->
                        Nothing

                    Just name ->
                        mainFrame id name [] True Nothing child
