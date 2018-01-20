module FS.Models exposing (..)

import RemoteData exposing (WebData)
import Dict exposing (Dict)
import FS.Models.Application exposing (Application(Application), initialApplication)
import FS.Models.Base
import FS.Models.Http


type alias Model =
    { operatingSystem : String
    , url : String
    , screenSize : FS.Models.Base.Size
    , applicationName : String
    , players : WebData (List FS.Models.Base.Player)
    , route : FS.Models.Base.Route
    , sessionId : Maybe String
    , response : FS.Models.Http.Response
    , application : Application
    }


initialModel : String -> String -> FS.Models.Base.Size -> String -> FS.Models.Base.Route -> Model
initialModel operatingSystem url screenSize applicationName route =
    let
        objectType =
            initialApplication applicationName
    in
        case objectType of
            FS.Models.Base.ObjectType application ->
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
