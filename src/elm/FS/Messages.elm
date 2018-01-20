module FS.Messages exposing (..)

import Http
import FS.Models.Base exposing (Player)
import FS.Models.Http exposing (Response)
import Navigation exposing (Location)
import RemoteData exposing (WebData)


type Msg
    = OnFetchPlayers (WebData (List Player))
    | OnLocationChange Location
    | ChangeLevel Player Int
    | OnPlayerSave (Result Http.Error Player)
    | OnFetchResponse (Result Http.Error Response)
