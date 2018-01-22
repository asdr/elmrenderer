module FS.Core.Http exposing (..)

import Http
import Xml.Decode
import FS.Models.Base exposing (Event, nullValue, servletUrl)
import FS.Models.Http exposing (RequestURL, RequestMethod, Request, Response)
import FS.Core.Types exposing (FormspiderType(Application))
import FS.Core.Utils exposing (toEventCode, toDeltaXML)


buildRequestBody : String -> String -> Request -> Http.Body
buildRequestBody objectType objectDtlType request =
    let
        applicationId =
            request.applicationId

        ( eventCode, instanceId ) =
            toEventCode request.events

        deltaXML =
            toDeltaXML request.delta
    in
        [ ( "in_applicationName_tx", request.applicationName )
        , ( "in_application_oid", (toString request.applicationId) )
        , ( "in_objectType_cd", objectType )
        , ( "in_panelDtlType_cd", objectDtlType )
        , ( "in_event_cd", eventCode )
        , ( "in_immediate_yn", "N" )
        , ( "in_objectIns_oid", instanceId )
        , ( "in_keyCombination_tx", request.keyCombination )
        , ( "in_delta_xml", deltaXML )
        , ( "in_delta_nr", (toString request.requestNumber) )
        ]
            |> Http.formDataBody


defaultRequestHeaders : Request -> List Http.Header
defaultRequestHeaders request =
    let
        sessionId =
            request.sessionId
    in
        case sessionId of
            Just sid ->
                [ Http.header "SessionId" sid
                ]

            Nothing ->
                []


createServletRequest : Request -> Http.Request Response
createServletRequest request =
    let
        objectType =
            case request.formspiderObject of
                Nothing ->
                    "UNKNOWN"

                Just fsObj ->
                    case fsObj of
                        Application _ ->
                            "Application"

                        _ ->
                            "UNKNOWN"

        requestBody =
            case request.formspiderObject of
                Nothing ->
                    Http.stringBody "text/plain" ""

                Just fsObj ->
                    case fsObj of
                        Application _ ->
                            buildRequestBody objectType nullValue request

                        _ ->
                            Http.stringBody "text/plain" ""
    in
        Http.request
            { method = "POST"
            , body = requestBody
            , expect = expectResponse responseDecoder
            , headers = List.append (defaultRequestHeaders request) request.headers
            , timeout = Nothing
            , url = servletUrl
            , withCredentials = False
            }


expectResponse : (Http.Response String -> Result String Response) -> Http.Expect Response
expectResponse decoder =
    Http.expectStringResponse (\response -> decoder response)


responseDecoder : Http.Response String -> Result String Response
responseDecoder response =
    let
        url =
            response.url

        status =
            response.status

        headers =
            Debug.log "Headers" response.headers

        xml =
            Xml.Decode.decode response.body
    in
        Result.Ok (Response url status headers (Result.toMaybe xml))
