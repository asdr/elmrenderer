module FS.Models.Http exposing (..)

import Dict exposing (Dict)
import Http
import Xml exposing (Value)
import FS.Core.Types exposing (FormspiderType)
import FS.Models.Base exposing (Event)
import FS.Models.Delta exposing (Delta)


type alias RequestURL =
    String


type alias RequestMethod =
    String


type alias Request =
    { applicationId : Int
    , applicationName : String
    , formspiderObject : Maybe FormspiderType
    , delta : Delta
    , headers : List Http.Header
    , events : List Event
    , keyCombination : String
    , requestNumber : Int
    , sessionId : Maybe String
    }


type alias Response =
    { url : String
    , status : { code : Int, message : String }
    , headers : Dict String String
    , xml : Maybe Value
    }
