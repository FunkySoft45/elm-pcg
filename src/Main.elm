import List exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Html exposing (Html)
import Element exposing (..)

import Random exposing (..)
import Debug exposing (..)

(w, h, u) = (800, 800, 10)

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

-- Step : 1
rad = 30 * u

c : Form      
c = filled red <| circle rad

-- generate
initSeed = initialSeed 316415
                
g = Random.int 0 10

gi : (Int, Seed)    
gi = Random.step g initSeed

randomList :  (Int, Seed) -> List Int -> List Int
randomList s l = if (length l) <= 10 then
          randomList (Random.step g (snd s)) (append [fst s] l) 
        else l
             
-- Random
{-
rad = 10
num = 1000

t = np.random.uniform(0.0, 2.0*np.pi, num)
r = rad * np.sqrt(np.random.uniform(0.0, 1.0, num))
x = r * np.cos(t)
y = r * np.sin(t)
-}

listTest = randomList gi []

-- main
main : Html a         
main =  let one = Debug.log "i " listTest in toHtml <| color lightGrey <| collage w h <| concat [gridLines, [ drawRoom room1 blue, drawRoom room2 yellow, drawRoom room3 green, c]]
       

