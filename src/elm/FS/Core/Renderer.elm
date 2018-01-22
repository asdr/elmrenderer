module FS.Core.Renderer exposing (..)

import Html exposing (..)
import Html.Attributes as HA
import FS.Core.Renderer.MainFrame as MainFrameRenderer
import FS.Core.Types exposing (FormspiderType(Application))
import FS.Messages exposing (Msg)
import FS.Models exposing (Model)


render : Model -> Html Msg
render model =
    let
        maybeMainFrame =
            case model.child of
                Nothing ->
                    Nothing

                Just formspiderObject ->
                    case formspiderObject of
                        Application aplicationProperties ->
                            aplicationProperties.child

                        _ ->
                            Nothing
    in
        MainFrameRenderer.render maybeMainFrame
