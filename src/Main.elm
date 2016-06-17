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

-- random point             
randomPoint : Generator (Int, Int)
randomPoint =
    pair (int -200 200) (int -100 100)

-- list random         
floatList : Generator (List Float)
floatList =
    list 10 (float 0 1)

intList : Generator (List Int)
intList =
    list 5 (int 0 100)

intPairs : Generator (List (Int, Int))
intPairs =
    list 10 randomPoint

         
-- http://stackoverflow.com/questions/5837572/generate-a-random-point-within-a-circle-uniformly
generatorStep1 : Int -> Int -> Generator (List (Float, Float))
generatorStep1 r n =
  Random.list n <| Random.pair (Random.float 0.0 (2.0 * pi)) (Random.map (\x -> x * toFloat r) (Random.float 0.0 1.0))

pointListIAC : Int -> Int -> Seed -> List (Int, Int)
pointListIAC r n s =
  List.map (\x -> (round <| (snd x) * cos (fst x), round <| (snd x) * sin (fst x))) <| fst (Random.step (generatorStep1 r n) s)

-- http://jsfiddle.net/b0sb5ogL/1/      
  
-- Random
{-
rad = 10
num = 1000

t = np.random.uniform(0.0, 2.0*np.pi, num)
r = rad * np.sqrt(np.random.uniform(0.0, 1.0, num))
x = r * np.cos(t)
y = r * np.sin(t)
-}

-- test = randomList gi []
-- test = fst <| Random.step intPairs initSeed

-- step 1 : Randomize

-- step 2 : Explose

-- step 3 : Selectionner room > seuil

-- step 4 : Triangulation 

-- step 5 : Minimum Spanning Tree

-- step 6 : Creer les couloirs

-- step 7 : Ajouter texture

-- test
test = pointListIAC rad 10 initSeed

-- main
main : Html a         
main =  let one = Debug.log "test " test in toHtml <| color lightGrey <| collage w h <| concat [gridLines, [ drawRoom room1 blue, drawRoom room2 yellow, drawRoom room3 green, c]]
       

