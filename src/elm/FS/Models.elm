module FS.Models exposing (..)

import RemoteData exposing (WebData)
import Dict exposing (Dict)
import FS.Core.Types exposing (FormspiderType(..), initialApplication, getId, getChild)
import FS.Models.Base exposing (Player, Route, Size)
import FS.Models.Http


type alias Model =
    { operatingSystem : String
    , url : String
    , screenSize : Size
    , applicationName : String
    , players : WebData (List Player)
    , route : Route
    , sessionId : Maybe String
    , initializedObjects : Dict Int FormspiderType
    , child : Maybe FormspiderType
    }


initialModel : String -> String -> Size -> String -> Route -> Model
initialModel operatingSystem url screenSize applicationName route =
    { operatingSystem = operatingSystem
    , url = url
    , screenSize = screenSize
    , applicationName = applicationName
    , players = RemoteData.Loading
    , route = route
    , sessionId = Nothing

    --, response =
    --    { url = ""
    --    , status = { code = 0, message = "not-sent" }
    --    , headers = Dict.empty
    --    , xml = Nothing
    --    }
    , initializedObjects = Dict.empty
    , child = initialApplication applicationName
    }


findParentObject : Maybe FormspiderType -> Maybe FormspiderType -> Maybe FormspiderType
findParentObject maybeObject maybeRoot =
    let
        maybeId =
            getId maybeObject

        maybeRootId =
            getId maybeRoot

        maybeChild =
            getChild maybeRoot
    in
        case maybeObject of
            Nothing ->
                Nothing

            Just object ->
                case object of
                    Application props ->
                        Nothing

                    _ ->
                        case maybeId of
                            Nothing ->
                                Nothing

                            Just id ->
                                let
                                    maybeChildId =
                                        getId maybeChild
                                in
                                    case maybeChildId of
                                        -- Nothing
                                        Nothing ->
                                            --case maybeRootId of
                                            --    Nothing ->
                                            --        Nothing
                                            --    Just rootId ->
                                            --        if id == rootId || rootId == -1 then
                                            --            Nothing
                                            --        else
                                            --            maybeRoot
                                            Nothing

                                        Just childId ->
                                            if id == childId then
                                                maybeRoot
                                            else
                                                findParentObject maybeObject maybeChild


findParent : Model -> Maybe FormspiderType -> Maybe FormspiderType
findParent model obj =
    let
        root =
            model.child
    in
        findParentObject obj root


updateBubble : Model -> Maybe FormspiderType -> Model
updateBubble model maybeObject =
    let
        maybeParent =
            maybeObject
                |> Debug.log "Models.updateBubble - child to find parent "
                |> findParent model
                |> Debug.log "Models.updateBubble - parent"
    in
        case maybeParent of
            Nothing ->
                case maybeObject of
                    Nothing ->
                        model

                    Just object ->
                        case object of
                            Application appProps ->
                                { model
                                    | child =
                                        (Application appProps)
                                            |> Debug.log "Models.updateBubble - application to set"
                                            |> Just
                                }
                                    |> Debug.log "Models.updateBubble - model after set"

                            _ ->
                                model

            Just parent ->
                case parent of
                    Application parentObj ->
                        Application
                            { parentObj
                                | child =
                                    maybeObject
                                        |> Debug.log "Models.updateBubble - mainframe to set"
                            }
                            |> Just
                            |> updateBubble model

                    _ ->
                        model
