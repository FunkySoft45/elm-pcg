-- ROOMS / LIST ROOMS utilities
module Grid exposing
  ( px
  , py
  , p
  , gline
  , gl
  , ghl
  , gwl
  , gridLines
  , toShape
  , toForm
  , getCenter
  , drawRoom
  , drawRooms
  , drawCell
  , floorTexture
  , drawRoomsTextured
  , drawRoomTextured
  , toFormTextured
  , w
  , h
  , u     
  )

import List exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Room exposing (..)

-----
-- Grid Size
(w, h) = (800, 800)

-- Unit 
u = 8


-- DRAW
px:  Float -> Float       
px x = -w/2 + u * x

py : Float -> Float       
py y = h/2 - u * y            
    
p : (Float, Float) -> (Float, Float)
p (x, y) = (px x, py y)

gline : LineStyle
gline = solid green        
        
gl : (Float, Float) -> (Float, Float) -> Form
gl (x1, y1) (x2, y2) = traced gline <| segment (p (x1, y1)) (p (x2, y2))   

ghl : List Form -> List Form
ghl l = 
  if (length l) <= (h//u) then
    ghl (append l [gl (0, toFloat <| (length l)) (w, toFloat <| (length l))])
  else l

gwl : List Form -> List Form
gwl l = 
  if (length l) <= (w//u) then
    gwl (append l [gl (toFloat <| (length l), 0) (toFloat <| (length l), h)])
  else l

gridLines : List Form
gridLines = concat [ghl [gl (0,0) (w, 0)], gwl [gl (0,0) (0, h)]]

toShape : Room -> Shape
toShape r = rect (toFloat (r.w * u)) (toFloat (r.h * u))

toForm : Room -> Form
toForm r = filled r.color <| toShape r

getCenter : Room -> (Float, Float)
getCenter r = ((fst r.c) + ((toFloat r.w)/2), (snd r.c)+ ((toFloat r.h)/2))            

drawRoom : Room -> Form
drawRoom r = move (p <| getCenter r) <| toForm r           

drawRooms : List Room -> List Form
drawRooms = 
  List.map
      (\x -> drawRoom x)

drawCell : (Float, Float) -> Color -> Form
drawCell c color = 
  move (p <| ((fst c) + 0.5, (snd c) + 0.5)) <| filled color <| rect (toFloat u) (toFloat u)

--- Texture
floorTexture : String
floorTexture = "http://localhost:8000/src/8x8_dungeon.png"

drawRoomsTextured : List Room -> List Form
drawRoomsTextured = 
  List.map
      (\x -> drawRoomTextured x)

drawRoomTextured : Room -> Form
drawRoomTextured r = move (p <| getCenter r) <| toFormTextured r  

toFormTextured : Room -> Form
toFormTextured r = textured floorTexture <| toShape r
