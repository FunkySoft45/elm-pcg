import List exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Html exposing (Html)
import Element exposing (..)

(w, h, u) = (800, 600, 20)

px:  Float -> Float       
px x = -w/2 + u * x

py : Float -> Float       
py y = h/2 - u * y            
    
p : (Float, Float) -> (Float, Float)
p (x, y) = (px x, py y)

gline : LineStyle
gline = solid red        
        
gl : (Float, Float) -> (Float, Float) -> Form
gl (x1, y1) (x2, y2) = traced gline <| segment (p (x1, y1)) (p (x2, y2))   

ghl : List Form -> List Form
ghl l = if (length l) <= (h//u) then
          ghl (append l [gl (0, toFloat <| (length l)) (w, toFloat <| (length l))])
        else l

gwl : List Form -> List Form
gwl l = if (length l) <= (w//u) then
          gwl (append l [gl (toFloat <| (length l), 0) (toFloat <| (length l), h)])
        else l

gridLines : List Form
gridLines = concat [ghl [gl (0,0) (w, 0)], gwl [gl (0,0) (0, h)]]

-- Room
type alias Room = { c : (Float, Float), w : Int, h : Int }

toShape : Room -> Shape
toShape r = rect (toFloat (r.w * u)) (toFloat (r.h * u))

toForm : Room -> Color -> Form
toForm r c = filled c <| toShape r

getCenter : Room -> (Float, Float)
getCenter r = ((fst r.c) + ((toFloat r.w)/2), (snd r.c)+ ((toFloat r.h)/2))            

drawRoom : Room -> Color -> Form
drawRoom r c = move (p <| getCenter r) <| toForm r c            

room1 : Room
room1 = {c=(0, 0), w=6, h=3}

room2 : Room
room2 = {c=(0, 4), w=6, h=3}

room3 : Room
room3 = {c=(0, 8), w=6, h=3}        

-- main
main : Html a         
main =  toHtml <| color lightGrey <| collage 800 600 <| concat [gridLines, [ drawRoom room1 blue, drawRoom room2 yellow, drawRoom room3 green ]]

