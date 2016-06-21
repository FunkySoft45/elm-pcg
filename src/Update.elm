module Update exposing (..)

import Messages exposing (..)
import Models exposing (..)
import Result exposing (..)
import String 

import Steps exposing (..)
    
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  
  case msg of
    Change newContent ->
      ( { model | seed = Result.withDefault  0 (String.toInt newContent) }, Cmd.none )
    Step1 ->
       ( { model | content = (drawStep1 model.seed) }, Cmd.none )
    Step2 ->
       ( { model | content = (drawStep2 model.seed) }, Cmd.none )         
    Step3 ->
       ( { model | content = (drawStep3 model.seed) }, Cmd.none )         
    Step4 ->
       ( { model | content = (drawStep4 model.seed) }, Cmd.none )         
    Step5 ->
       ( { model | content = (drawStep5 model.seed) }, Cmd.none )        
