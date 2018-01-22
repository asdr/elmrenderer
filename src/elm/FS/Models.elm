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

    --, response : FS.Models.Http.Response
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
                            case maybeRootId of
                                Nothing ->
                                    Nothing

                                Just rootId ->
                                    if id == rootId || rootId == -1 then
                                        Nothing
                                    else
                                        maybeRoot

                        Just childId ->
                            if id == childId then
                                maybeRoot
                            else
                                findParentObject maybeObject maybeChild


findParent : Maybe FormspiderType -> Model -> Maybe FormspiderType
findParent obj model =
    let
        root =
            model.child
    in
        findParentObject (Debug.log "obj" obj) (Debug.log "root" root)


updateBubble : Model -> Maybe FormspiderType -> Model
updateBubble model maybeObject =
    let
        maybeParent =
            Debug.log "maybeParent" <| findParent maybeObject model
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
                                    | child = Just (Application appProps)
                                }

                            _ ->
                                model

            Just parent ->
                case parent of
                    Application parentObj ->
                        Application
                            { parentObj
                                | child = maybeObject
                            }
                            |> Just
                            |> updateBubble model

                    _ ->
                        model
