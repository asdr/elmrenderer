module FS.Commands exposing (..)

import Http
import Json.Decode as JsonDecode
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as Encode
import RemoteData
import FS.Core.Http exposing (createServletRequest)
import FS.Messages exposing (Msg)
import FS.Core.Types exposing (FormspiderType(Application), initialApplication)
import FS.Models.Base exposing (Event(Open), Player, nullValue, servletUrl)
import FS.Models.Http exposing (Request)


openApplication : String -> Cmd Msg
openApplication applicationName =
    let
        request =
            { applicationId = -1
            , applicationName = applicationName
            , formspiderObject = initialApplication applicationName
            , delta = []
            , headers = []
            , events = [ Open -1 ]
            , keyCombination = nullValue
            , requestNumber = 1
            , sessionId = Nothing
            }
    in
        createServletRequest request
            |> Http.send FS.Messages.OnFetchResponse


fetchPlayers : Cmd Msg
fetchPlayers =
    Http.get fetchPlayersUrl playersDecoder
        |> RemoteData.sendRequest
        |> Cmd.map FS.Messages.OnFetchPlayers


fetchPlayersUrl : String
fetchPlayersUrl =
    "http://localhost:4000/players"


savePlayerUrl : String -> String
savePlayerUrl playerId =
    "http://localhost:4000/players/" ++ playerId


savePlayerRequest : Player -> Http.Request Player
savePlayerRequest player =
    Http.request
        { body = playerEncoder player |> Http.jsonBody
        , expect = Http.expectJson playerDecoder
        , headers = []
        , method = "PATCH"
        , timeout = Nothing
        , url = savePlayerUrl player.id
        , withCredentials = False
        }


savePlayerCmd : Player -> Cmd Msg
savePlayerCmd player =
    savePlayerRequest player
        |> Http.send FS.Messages.OnPlayerSave



-- DECODERS


playersDecoder : JsonDecode.Decoder (List Player)
playersDecoder =
    JsonDecode.list playerDecoder


playerDecoder : JsonDecode.Decoder Player
playerDecoder =
    decode Player
        |> required "id" JsonDecode.string
        |> required "name" JsonDecode.string
        |> required "level" JsonDecode.int


requestEncoder : Request -> Encode.Value
requestEncoder request =
    Encode.object []


playerEncoder : Player -> Encode.Value
playerEncoder player =
    let
        attributes =
            [ ( "id", Encode.string player.id )
            , ( "name", Encode.string player.name )
            , ( "level", Encode.int player.level )
            ]
    in
        Encode.object attributes
