module View exposing (..)


import Models exposing (..)

import Html exposing (Html, Attribute, text, div, input, button)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Messages exposing (..)


import Collage exposing (..)
import Color exposing (..)

import Element exposing (..)


divStyle =
    Html.Attributes.style
        [ ( "color", "#FFF" )
        , ( "fontFamily", "Tahoma" )
        ]


divContainer =
    Html.Attributes.style
        [ ( "backgroundColor", "#FFF" )
        , ( "display", "flex" )
        , ( "height", "100%" )
        ]


divFixed =
    Html.Attributes.style
        [ ( "backgroundColor", "#4F628E" )
        , ( "width", "200px" )
        ]


divFlexItem =
    Html.Attributes.style
        [ ( "backgroundColor", "#FFF" )
        , ( "flexGrow", "1" )
        , ( "color", "#000")    
        ]
      
view : Model -> Html Msg
view model =
    div [ divStyle, divContainer ]
        [ div [ divStyle, divFixed ]
            [ input [ placeholder "Seed...", onInput Change ] []
            , button [ onClick Step1 ] [ Html.text "step 1" ]
            , button [ onClick Step2 ] [ Html.text "step 2" ]
            , button [ onClick Step3 ] [ Html.text "step 3" ]
            , button [ onClick Step4 ] [ Html.text "step 4" ]
            , button [ onClick Step5 ] [ Html.text "step 5" ]                     
            ]
        , div [ divStyle, divFlexItem ] [ toHtml <| collage 800 800 <| model.content ] 
        ]
