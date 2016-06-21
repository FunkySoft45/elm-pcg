module Models exposing ( Model )

import List
import Collage exposing (Form)

type alias Model =
  { content : List Form
  , seed : Int          
  }
