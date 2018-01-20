module FS.Models.Delta exposing (..)


type DeltaItem objectType value
    = DeltaItem Int objectType String value


type alias Delta =
    List DeltaItem
