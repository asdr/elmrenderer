module FS.Models.Application exposing (..)

import Dict exposing (Dict)
import FS.Models.Base exposing (CommonProperties, ObjectType(..), nullValue)
import FS.Models.MainFrame exposing (MainFrame(MainFrame))


type Application
    = Application (ApplicationProperties CommonProperties)


type alias ApplicationProperties common =
    { common
        | cssFile : Maybe String
        , language : ( String, String )
        , mainFrame : Maybe MainFrame
        , events : List String
    }


initialApplication : String -> ObjectType Application
initialApplication name =
    ObjectType
        (Application
            { id = -1
            , name = name
            , objectType = nullValue
            , cssFile = Nothing
            , language = ( "en", "US" )
            , mainFrame = Nothing
            , events = []
            }
        )
