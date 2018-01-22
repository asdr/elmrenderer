module FS.Core.Types exposing (..)

import FS.Models.Base exposing (nullValue)


type alias CommonProperties =
    { id : Int
    , name : String
    , objectType : String
    , events : List String
    , child : Maybe FormspiderType
    }


type FormspiderType
    = Application (ApplicationProperties CommonProperties)
    | MainFrame (MainFrameProperties CommonProperties)
    | Panel Panel
    | Component Component


type alias ApplicationProperties common =
    { common
        | cssFile : Maybe String
        , language : ( String, String )
    }


type alias MainFrameProperties common =
    { common
        | visible : Bool
        , theme : Maybe String
    }


type Panel
    = SimplePanel (SimplePanelProperties CommonProperties)
    | RepeatPanel (RepeatPanelProperties CommonProperties)
    | TabPanel (TabPanelProperties CommonProperties)


type alias SimplePanelProperties common =
    { common
        | panelType : String
        , styleClass : Maybe String
        , visible : Bool
        , header : Maybe Header
    }


type alias RepeatPanelProperties common =
    { common
        | panelType : String
        , styleClass : String
        , visible : Bool
        , datasourceId : Int
        , header : Header
    }


type alias TabPanelProperties common =
    { common
        | panelType : String
        , styleClass : String
        , visible : Bool
        , tabs : List Tab
    }



{- Tab = (order, title, panelName, panel) -}


type alias Tab =
    ( Int, String, String, Panel )


type alias Header =
    { left : List Component
    , center : List Component
    , right : List Component
    }


type Component
    = Button (ButtonProperties CommonProperties)


type alias ButtonProperties component =
    { component
        | styleClass : String
        , label : String
    }


type Layout
    = Layout (LayoutProperties CommonProperties)


type alias LayoutProperties common =
    { common
        | layoutType : String
    }


getId : Maybe FormspiderType -> Maybe Int
getId maybeObject =
    case maybeObject of
        Nothing ->
            Nothing

        Just object ->
            case object of
                Application props ->
                    Just props.id

                MainFrame props ->
                    Just props.id

                Panel panel ->
                    case panel of
                        SimplePanel props ->
                            Just props.id

                        RepeatPanel props ->
                            Just props.id

                        TabPanel props ->
                            Just props.id

                Component component ->
                    case component of
                        Button props ->
                            Just props.id


getChild : Maybe FormspiderType -> Maybe FormspiderType
getChild maybeObject =
    case maybeObject of
        Nothing ->
            Nothing

        Just object ->
            case object of
                Application props ->
                    props.child

                MainFrame props ->
                    props.child

                Panel panel ->
                    case panel of
                        SimplePanel props ->
                            props.child

                        RepeatPanel props ->
                            props.child

                        TabPanel props ->
                            props.child

                Component component ->
                    case component of
                        Button props ->
                            props.child


findObject : Maybe Int -> Maybe FormspiderType -> Maybe FormspiderType
findObject maybeId maybeRoot =
    case maybeId of
        Nothing ->
            Nothing

        Just id ->
            let
                maybeRootId =
                    getId maybeRoot
            in
                case (Debug.log "maybeRootId" maybeRootId) of
                    Nothing ->
                        Nothing

                    Just rootId ->
                        if id == rootId || rootId == -1 then
                            maybeRoot
                        else
                            getChild maybeRoot
                                |> findObject maybeId


initialApplication : String -> Maybe FormspiderType
initialApplication name =
    Just <|
        Application
            { id = -1
            , name = name
            , objectType = nullValue
            , cssFile = Nothing
            , language = ( "en", "US" )
            , child = Nothing
            , events = []
            }


mainFrame : Int -> String -> List String -> Bool -> Maybe String -> Maybe FormspiderType -> Maybe FormspiderType
mainFrame id name events visible theme child =
    Just <|
        MainFrame
            { id = id
            , name = name
            , objectType = "MainFrame"
            , events = events
            , child = child
            , visible = visible
            , theme = theme
            }
