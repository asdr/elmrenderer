module FS.Models.Actions exposing (..)

import Dict exposing (Dict)
import Xml


type Action a
    = Action a


type ApplicationAction
    = ApplicationOpen (Maybe Int)


type MainFrameAction
    = MainFrameInitialize (Maybe Int)
