module FS.Update.Panel exposing (..)

import Xml
import FS.Core.Types exposing (FormspiderType(Panel), Panel(SimplePanel, RepeatPanel, TabPanel))


initialize : Xml.Value -> Maybe Int -> Maybe FormspiderType
initialize response maybePanelId =
    Just <|
        Panel <|
            SimplePanel
                { id = 222
                , name = "mainPanel"
                , objectType = "Panel"
                , events = []
                , child = Nothing
                , panelType = "SimplePanel"
                , styleClass = Nothing
                , visible = True
                , header = Nothing
                }
