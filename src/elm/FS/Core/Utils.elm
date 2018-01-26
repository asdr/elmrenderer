module FS.Core.Utils exposing (..)

import FS.Models.Base exposing (Event(Open))
import FS.Models.Delta exposing (Delta)


concatWithSeparator : String -> String -> String -> String
concatWithSeparator separator item1 item2 =
    if String.isEmpty item2 then
        item1
    else
        item1 ++ separator ++ item2


joinList : List String -> String -> String
joinList l separator =
    List.foldl (concatWithSeparator separator) "" l


toEventCode : List Event -> ( String, String )
toEventCode events =
    let
        ( l1, l2 ) =
            (List.map
                (\event ->
                    case event of
                        Open id ->
                            ( "Open", toString id )
                )
                events
            )
                |> List.unzip
    in
        ( joinList l1 ";", joinList l2 ";" )


toDeltaXML : Delta -> String
toDeltaXML delta =
    """<delta><application rendererVersion="1.9.0" rendererType="JS" os.name="MacIntel" url="http://formspideronline.com:60060/formspider/" screenWidth="1440" screenHeight="144"/></delta>"""


toBooleanFromYN : Maybe String -> Maybe Bool
toBooleanFromYN maybeValue =
    case maybeValue of
        Nothing ->
            Nothing

        Just value ->
            case value of
                "Y" ->
                    Just True

                _ ->
                    Just False
