module FS.Models.Http exposing (..)

import Dict exposing (Dict)
import Http
import Xml exposing (Value)
import FS.Models.Core


type alias RequestURL =
    String


type alias RequestMethod =
    String


type alias Request a =
    { applicationId : Int
    , applicationName : String
    , objectType : FS.Models.Core.ObjectType a
    , delta : FS.Models.Core.Delta
    , headers : List Http.Header
    , events : List FS.Models.Core.Event
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
