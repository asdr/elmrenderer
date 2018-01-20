module FS.Update.Application exposing (..)

import Dict exposing (Dict, keys)
import List
import Xml
import Xml.Query
import FS.Models exposing (Model)
import FS.Models.Application exposing (Application(Application))
import FS.Models.Actions exposing (Action(Action), ApplicationAction(ApplicationOpen))


update : Model -> Action ApplicationAction -> Model
update model action =
    case action of
        Action appAction ->
            case appAction of
                ApplicationOpen actionAttrs ->
                    let
                        application =
                            model.application
                    in
                        { model
                            | application = updateApplicationModel application actionAttrs
                        }


updateApplicationModel : Application -> Dict String Xml.Value -> Application
updateApplicationModel app attributes =
    let
        attributeNames =
            Dict.keys attributes
    in
        updateModel attributeNames attributes app


updateModel : List String -> Dict String Xml.Value -> Application -> Application
updateModel keys attributes app =
    let
        key =
            Maybe.withDefault "" (List.head keys)

        val =
            Dict.get key attributes

        rest =
            Maybe.withDefault [] (List.tail keys)
    in
        if List.length rest == 0 then
            setProperty key val app
        else
            app
                |> setProperty key val
                |> updateModel rest attributes


setProperty : String -> Maybe Xml.Value -> Application -> Application
setProperty key value app =
    case value of
        Just theValue ->
            let
                intValue =
                    Xml.Query.int theValue
                        |> Result.toMaybe
                        |> Maybe.withDefault -1
            in
                case app of
                    Application props ->
                        case key of
                            "oid" ->
                                Application { props | id = intValue }

                            _ ->
                                app

        _ ->
            app
