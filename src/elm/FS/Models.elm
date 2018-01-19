module FS.Models exposing (..)

import RemoteData exposing (WebData)
import Dict exposing (Dict)
import FS.Models.Core
import FS.Models.Http


type alias Model =
    { operatingSystem : String
    , url : String
    , screenSize : FS.Models.Core.Size
    , applicationName : String
    , players : WebData (List FS.Models.Core.Player)
    , route : FS.Models.Core.Route
    , sessionId : Maybe String
    , response : FS.Models.Http.Response
    , application : FS.Models.Core.Application
    }


initialModel : String -> String -> FS.Models.Core.Size -> String -> FS.Models.Core.Route -> Model
initialModel operatingSystem url screenSize applicationName route =
    let
        objectType =
            FS.Models.Core.initialApplication applicationName
    in
        case objectType of
            FS.Models.Core.ObjectType application ->
                { operatingSystem = operatingSystem
                , url = url
                , screenSize = screenSize
                , applicationName = applicationName
                , players = RemoteData.Loading
                , route = route
                , sessionId = Nothing
                , response =
                    { url = ""
                    , status = { code = 0, message = "not-sent" }
                    , headers = Dict.empty
                    , xml = Nothing
                    }
                , application = application
                }
