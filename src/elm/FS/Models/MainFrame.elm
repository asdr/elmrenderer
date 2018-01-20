module FS.Models.MainFrame exposing (..)

import FS.Models.Base exposing (CommonProperties)


type MainFrame
    = MainFrame MainFrameProperties


type alias MainFrameProperties =
    { common : CommonProperties
    }
