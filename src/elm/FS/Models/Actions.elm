module FS.Models.Actions exposing (..)

import Dict exposing (Dict)
import Xml


type Action a
    = Action a


type ApplicationAction
    = ApplicationOpen (Dict String Xml.Value)
