module FS.Update.Panel exposing (..)

import Dict
import Xml
import FS.Models exposing (Model)
import FS.Core.Types exposing (FormspiderType(Panel), Panel(SimplePanel, RepeatPanel, TabPanel))


initialize : Xml.Value -> Model -> Maybe Int -> ( Model, Maybe FormspiderType )
initialize response model maybePanelId =
    let
        maybeInitializedPanel =
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
    in
        case maybePanelId of
            Nothing ->
                ( model, Nothing )

            Just panelId ->
                case maybeInitializedPanel of
                    Nothing ->
                        ( model, Nothing )

                    Just initializedPanel ->
                        ( { model
                            | initializedObjects = Dict.insert panelId initializedPanel model.initializedObjects
                          }
                        , maybeInitializedPanel
                        )
