import List exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Debug exposing (..)


import Html.App
import Html exposing (Html, Attribute, text, div, input, button)
 

import Messages exposing (..)

import Models exposing (..)
import View exposing (..)

import Update exposing (update)
   

init : ( Model, Cmd Msg )
init =
    ( { content = [], seed = 0 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
      
