module FS.Models.Actions exposing (..)


type Action a
    = Action a


type ApplicationAction
    = ApplicationOpen (Maybe Int)


type MainFrameAction
    = MainFrameInitialize (Maybe Int)
    | MainFrameSetVisible (Maybe Int) (Maybe Bool)
