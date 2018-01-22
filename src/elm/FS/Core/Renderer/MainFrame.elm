module FS.Core.Renderer.MainFrame exposing (..)

import Html exposing (..)
import Html.Attributes as HA
import FS.Core.Types exposing (FormspiderType(MainFrame), CommonProperties, MainFrameProperties)
import FS.Messages exposing (Msg)


render : Maybe FormspiderType -> Html Msg
render maybeFormspiderObject =
    case maybeFormspiderObject of
        Nothing ->
            div [] []

        Just formspiderObject ->
            case formspiderObject of
                MainFrame properties ->
                    renderMainFrame properties

                _ ->
                    div [] []


renderMainFrame : MainFrameProperties CommonProperties -> Html Msg
renderMainFrame properties =
    div
        [ HA.class "fs-mainframe"
        , HA.name properties.name
        , HA.id (toString properties.id)
        ]
        []
