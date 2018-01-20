module FS.Models.Component exposing (..)

import FS.Models.Base exposing (CommonProperties, ObjectType(..), nullValue)


type Component
    = Component ComponentProperties


type alias ComponentProperties =
    { common : CommonProperties
    }
