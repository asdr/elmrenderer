module FS.Models.Core exposing (..)


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



--type ObjectType
--    = Application ApplicationProperties
--    | Panel PanelProperties
--    | Component ComponentProperties


type ObjectType a
    = ObjectType a


type Application
    = Application ApplicationProperties


type MainFrame
    = MainFrame MainFrameProperties


type Panel
    = Panel PanelProperties


type Component
    = Component ComponentProperties


type Event
    = Open Int


type alias CommonProperties =
    { id : Int
    , name : String
    , objectType : String
    }


type alias ApplicationProperties =
    { common : CommonProperties
    }


type alias MainFrameProperties =
    { common : CommonProperties
    }


type alias PanelProperties =
    { common : CommonProperties
    }


type alias ComponentProperties =
    { common : CommonProperties
    }


type DeltaItem objectType value
    = DeltaItem Int objectType String value


type alias Delta =
    List DeltaItem


initialApplication : String -> ObjectType Application
initialApplication name =
    ObjectType (Application (ApplicationProperties (CommonProperties -1 name nullValue)))
