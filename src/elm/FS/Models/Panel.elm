module FS.Models.Panel exposing (..)

import FS.Models.Base exposing (CommonProperties, ObjectType(..), nullValue)


type Panel
    = Panel PanelProperties


type alias PanelProperties =
    { common : CommonProperties
    }
