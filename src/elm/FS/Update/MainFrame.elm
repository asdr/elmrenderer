module FS.Update.MainFrame exposing (update)

import Dict exposing (Dict)
import Xml
import Xml.Query exposing (tags)
import FS.Core.Types exposing (CommonProperties, MainFrameProperties, FormspiderType(Application, MainFrame), findObject, mainFrame)
import FS.Models exposing (Model, updateBubble)
import FS.Models.Actions exposing (Action(Action), MainFrameAction(MainFrameInitialize, MainFrameSetVisible))
import FS.Update.Panel as PanelUpdate


update : Xml.Value -> Model -> Action MainFrameAction -> Model
update response model action =
    case action of
        Action appAction ->
            case appAction of
                MainFrameInitialize maybeId ->
                    let
                        maybeMainFrame =
                            findObject maybeId model.child
                    in
                        case maybeMainFrame of
                            Nothing ->
                                case maybeId of
                                    Nothing ->
                                        model

                                    Just id ->
                                        let
                                            maybeInitializedMainFrame =
                                                initialize response maybeId
                                        in
                                            case maybeInitializedMainFrame of
                                                Nothing ->
                                                    model

                                                Just initializedMainFrame ->
                                                    { model
                                                        | initializedObjects = Dict.insert id initializedMainFrame model.initializedObjects
                                                    }

                            Just mainFrame ->
                                model

                MainFrameSetVisible maybeId maybeVisible ->
                    case maybeId of
                        Nothing ->
                            model

                        Just id ->
                            let
                                maybeInitializedMainFrame =
                                    Dict.get id model.initializedObjects
                            in
                                case maybeInitializedMainFrame of
                                    Nothing ->
                                        model

                                    Just initializedMainFrame ->
                                        case initializedMainFrame of
                                            MainFrame props ->
                                                let
                                                    modifiedMainFrame =
                                                        MainFrame { props | visible = Maybe.withDefault True maybeVisible }

                                                    maybeApplicationObject =
                                                        model.child
                                                in
                                                    case maybeApplicationObject of
                                                        Nothing ->
                                                            model

                                                        Just applicationObject ->
                                                            case applicationObject of
                                                                Application props ->
                                                                    let
                                                                        modifiedApplication =
                                                                            Application { props | child = Just modifiedMainFrame }
                                                                    in
                                                                        { model
                                                                            | child = Just modifiedApplication
                                                                        }

                                                                _ ->
                                                                    model

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

        maybeViisble =
            attributes
                |> Dict.get ("display")
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
