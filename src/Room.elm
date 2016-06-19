-- ROOMS / LIST ROOMS utilities
module Room exposing
  ( Room
  , defaultRoom
  , changeColor
  , flagRoom
  , getFirst
  )

import Color exposing (..)
import Maybe exposing (..)
import List exposing (..)

type alias Room = { c : (Float, Float), w : Int, h : Int, color : Color, isRoom : Bool }

defaultRoom : Room
defaultRoom = 
  { c = (0,0)
  , h = 0
  , w = 0
  , color = white
  , isRoom = False
  }


changeColor : Room -> Color -> Room 
changeColor r c = 
  { c = r.c
  , w = r.w
  , h = r.h
  , color = c
  , isRoom = r.isRoom
  }

flagRoom : Room -> Room
flagRoom r = 
   { c =r.c
  , w = r.w
  , h = r.h
  , color = r.color
  , isRoom = True
  }

getFirst : List Room -> Room
getFirst l = 
  withDefault defaultRoom (head l)
