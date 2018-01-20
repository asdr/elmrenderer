module FS.Core.Renderer exposing (..)

import Html exposing (..)
import FS.Messages exposing (Msg)
import FS.Models.Http exposing (Response)


render : Response -> Html Msg
render response =
    let
        maybeXml =
            response.xml
    in
        case maybeXml of
            Nothing ->
                div [] [ text "Nothing" ]

            Just xml ->
                div [] [ text (toString xml) ]
