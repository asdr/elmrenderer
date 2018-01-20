module FS.Update.Application exposing (update)

import Dict exposing (Dict, keys)
import List
import Xml
import Xml.Encode exposing (null)
import Xml.Query
import FS.Models exposing (Model)
import FS.Models.Application exposing (Application(Application))
import FS.Models.Actions exposing (Action(Action), ApplicationAction(ApplicationOpen))


update : Xml.Value -> Model -> Action ApplicationAction -> Model
update response model action =
    case action of
        Action appAction ->
            case appAction of
                ApplicationOpen actionAttrs ->
                    let
                        application =
                            model.application
                    in
                        { model
                            | application =
                                (Dict.get "oid" actionAttrs
                                    |> Maybe.withDefault (Xml.IntNode -1)
                                    |> Xml.Query.int
                                    |> Result.toMaybe
                                    |> getAttributes response
                                    |> updateApplicationModel application
                                )
                        }


getAttributes : Xml.Value -> Maybe Int -> Dict String Xml.Value
getAttributes response maybeApplicationId =
    case maybeApplicationId of
        Nothing ->
            Dict.empty

        Just applicationId ->
            let
                appNode =
                    Xml.Query.tags "logical" response
                        |> List.head
                        |> Maybe.withDefault null
                        |> Xml.Query.tags "application"
                        |> List.head
            in
                case appNode of
                    Nothing ->
                        Dict.empty

                    Just node ->
                        case node of
                            Xml.Tag name attributes children ->
                                attributes

                            _ ->
                                Dict.empty


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
            case app of
                Application props ->
                    case key of
                        "oid" ->
                            Application
                                { props
                                    | id =
                                        (Xml.Query.int theValue
                                            |> Result.toMaybe
                                            |> Maybe.withDefault -1
                                        )
                                }

                        "name" ->
                            Application
                                { props
                                    | name =
                                        (Xml.Query.string theValue
                                            |> Result.toMaybe
                                            |> Maybe.withDefault ""
                                        )
                                }

                        "cssFile" ->
                            Application
                                { props
                                    | cssFile =
                                        (Xml.Query.string theValue
                                            |> Result.toMaybe
                                        )
                                }

                        _ ->
                            app

        _ ->
            app
