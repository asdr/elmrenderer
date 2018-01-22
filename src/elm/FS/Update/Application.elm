module FS.Update.Application exposing (update)

import Dict exposing (Dict, keys)
import List
import Xml
import Xml.Encode exposing (null)
import Xml.Query
import FS.Models exposing (Model, updateBubble)
import FS.Core.Types exposing (FormspiderType(Application), findObject)
import FS.Models.Actions exposing (Action(Action), ApplicationAction(ApplicationOpen))


update : Xml.Value -> Model -> Action ApplicationAction -> Model
update response model action =
    case action of
        Action appAction ->
            case appAction of
                ApplicationOpen applicationId ->
                    let
                        maybeApplication =
                            Debug.log "maybeApplication" (findObject (Debug.log "applicationId" applicationId) model.child)
                    in
                        case maybeApplication of
                            Nothing ->
                                model

                            Just application ->
                                case application of
                                    Application app ->
                                        applicationId
                                            |> getApplicationInformation response
                                            |> updateApplicationModel application
                                            |> Just
                                            |> updateBubble model

                                    _ ->
                                        model


getApplicationInformation : Xml.Value -> Maybe Int -> Dict String Xml.Value
getApplicationInformation response maybeApplicationId =
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
                                Debug.log ("Children: " ++ (toString children)) attributes

                            _ ->
                                Dict.empty


updateApplicationModel : FormspiderType -> Dict String Xml.Value -> FormspiderType
updateApplicationModel app attributes =
    let
        attributeNames =
            Dict.keys attributes
    in
        updateModel attributeNames attributes app


updateModel : List String -> Dict String Xml.Value -> FormspiderType -> FormspiderType
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


setProperty : String -> Maybe Xml.Value -> FormspiderType -> FormspiderType
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

        _ ->
            app
