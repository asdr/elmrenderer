module FS.Models.Http exposing (..)

import Dict exposing (Dict)
import Http
import Xml exposing (Value)
import FS.Models.Base exposing (Event, ObjectType)
import FS.Models.Delta exposing (Delta)


type alias RequestURL =
    String


type alias RequestMethod =
    String


type alias Request a =
    { applicationId : Int
    , applicationName : String
    , objectType : ObjectType a
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
