module FS exposing (..)

import Navigation exposing (Location)
import FS.Models exposing (Model, initialModel)
import FS.Models.Base exposing (Size)
import FS.Commands exposing (fetchPlayers, openApplication)
import FS.Messages exposing (Msg)
import FS.Routing
import FS.Subscriptions exposing (subscriptions)
import FS.Update exposing (update)
import FS.View exposing (view)


type alias Flags =
    { operatingSystem : String
    , url : String
    , screenSize : Size
    , applicationName : String
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        currentRoute =
            FS.Routing.parseLocation location
    in
        ( initialModel flags.operatingSystem flags.url flags.screenSize flags.applicationName currentRoute, openApplication flags.applicationName )


main : Program Flags Model Msg
main =
    Navigation.programWithFlags FS.Messages.OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
