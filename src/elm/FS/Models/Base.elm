module FS.Models.Base exposing (..)


rendererType : String
rendererType =
    "ElmRenderer"


rendererVersion : String
rendererVersion =
    "1.0.0"


baseUrl : String
baseUrl =
    "http://formspideronline.com:60060/formspider/"


servletUrl : String
servletUrl =
    baseUrl ++ "bdfservlet"


nullValue : String
nullValue =
    "{NULL_VALUE}"


type alias Size =
    { width : Float
    , height : Float
    }


type alias Player =
    { id : String
    , name : String
    , level : Int
    }


type Route
    = DefaultRoute
    | PlayersRoute
    | PlayerRoute String
    | NotFoundRoute


type Event
    = Open Int
