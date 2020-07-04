module Main exposing (..)

import Html exposing (Html, text)


type alias Date =
    Int


type alias Entry =
    { date : Date
    , amount : Int
    , tags : List String
    , description : String
    }


type alias Report =
    List Entry


main : Html msg
main =
    text "Hello World!"
