module FS.Core.Http exposing (..)

import Http
import Xml.Decode
import FS.Models.Application exposing (Application(Application))
import FS.Models.Base exposing (Event, ObjectType, nullValue, servletUrl)
import FS.Models.Http exposing (RequestURL, RequestMethod, Request, Response)
import FS.Core.Utils exposing (toEventCode, toDeltaXML)


buildRequestBody : String -> String -> Request a -> Http.Body
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


defaultRequestHeaders : Request a -> List Http.Header
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


createServletRequest : Request a -> Http.Body -> Http.Request Response
createServletRequest request body =
    Http.request
        { method = "POST"
        , body = body
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


buildApplicationRequestBody : Request Application -> Http.Body
buildApplicationRequestBody request =
    buildRequestBody "Application" nullValue request


createApplicationServletRequest : Request Application -> Http.Request Response
createApplicationServletRequest request =
    buildApplicationRequestBody request
        |> createServletRequest request
