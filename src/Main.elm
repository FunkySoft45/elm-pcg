import List exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Maybe exposing (..)
import Random exposing (..)
import Debug exposing (..)

----------------------------------------------------------------------------------------------
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

---

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

----------------------------------------------------------------------------------------------
-- ROOMS / LIST ROOMS utilities

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


----------------------------------------------------------------------------------------------
-- step 1 : Randomize
              
generatorColors : Int -> Generator (List (Color.Color))
generatorColors n =
  Random.list n <| 
    Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)

generatorRoomSize : Int -> Generator (List (Int, Int))
generatorRoomSize n =
  Random.list n <|
    Random.pair 
      (Random.int 5 20) 
      (Random.int 5 20)

generatorPointsInACircle : Float -> Int -> Generator (List (Float, Float))
generatorPointsInACircle r n =
  Random.list n <| 
    Random.pair 
      (Random.map (\x -> x * 2 * pi) (Random.float 0.0 1.0)) 
      (Random.map (\x -> sqrt x) (Random.float 0.0 1.0))

randomPointsInACircle : Float -> Int -> Seed -> List (Float, Float)
randomPointsInACircle r n s =
  List.map 
      (\x -> (toFloat <| round <| (snd x * r * cos (fst x) + (w/u/2)), toFloat <| round <| snd x * r * sin (fst x) + (h/u/2))) 
      (fst (Random.step (generatorPointsInACircle r n) s))

combineRoom: (Float, Float) -> (Int, Int) -> Color -> Room
combineRoom p s col =
    { c = p
    , w = fst s
    , h = snd s
    , color = col
    , isRoom = False
    }  

generateRooms : Int -> Int -> List Room
generateRooms n s =
  List.map3 combineRoom
    (randomPointsInACircle rad n (initialSeed (s)))
    (fst (Random.step (generatorRoomSize n) (initialSeed (s + 198))))
    (fst (Random.step (generatorColors n) (initialSeed (s + 157))))


----------------------------------------------------------------------------------------------
-- step 2 : Explose

-- Move X coord 1 unit away from center
moveX : Float -> Float
moveX x =
  if (x <= w/u/2) then
    x - 1
  else
    x + 1

-- Move Y coord 1 unit away from center
moveY: Float -> Float
moveY y =
  if (y <= h/u/2) then
    y - 1
  else
    y + 1

moveXorY: (Float, Float) -> (Float, Float)
moveXorY p = 
   if ((getAngle p) >= (-1*pi/8) && (getAngle p) <= (pi/8)) then
    (moveX (fst p), (snd p))
  else if ((getAngle p) > (pi/8) && (getAngle p) <= (3*pi/8)) then
    (moveX (fst p), moveY (snd p))
  else if ((getAngle p) > (3*pi/8) && (getAngle p) <= (5*pi/8)) then
    ((fst p), moveY (snd p))
  else if ((getAngle p) > (5*pi/8) && (getAngle p) <= (7*pi/8)) then
    (moveX (fst p), moveY (snd p))
  else if ((getAngle p) > (7*pi/8) && (getAngle p) <= (pi)) then
    (moveX (fst p), (snd p))
  else if ((getAngle p) < (-1*pi/8) && (getAngle p) >= (-3*pi/8)) then
    (moveX (fst p), moveY (snd p))
  else if ((getAngle p) < (-3*pi/8) && (getAngle p) >= (-5*pi/8)) then
    ((fst p), moveY (snd p))
  else 
    (moveX (fst p), moveY (snd p))

getAngle: (Float, Float) -> Float
getAngle p =
  atan2 ((h/u/2) - (snd p)) ((fst p) - (w/u/2)) 

-- Move a room by 1 unit away from the center 
moveOne : Room -> Room
moveOne r =
  { c = moveXorY r.c
  , w = r.w
  , h = r.h
  , color = r.color
  , isRoom = False
  }

-- Check if 2 rooms overlap
overlap : Room -> Room -> Bool
overlap r1 r2 = 
  not <|  
    (fst (r1.c) + (toFloat r1.w) <= fst (r2.c)) || (fst (r2.c) + (toFloat r2.w) <= fst (r1.c)) 
      ||
    (snd (r1.c) + (toFloat r1.h) <= snd (r2.c)) || (snd (r2.c) + (toFloat r2.h) <= snd (r1.c))

-- Push one room away from another until it doesnt overlap anymore
push : Room -> Room -> Room
push r1 r2 = 
  if (overlap r1 r2) then
    push (moveOne r1) r2
  else
    r1

pushOthers : Room -> List Room -> List Room
pushOthers r l = 
  List.map (\x ->  push x r) l

-- Original List -> Acc (rooms done) -> Acc (Remaining) -> New List
positionAll : List Room -> List Room -> List Room -> List Room
positionAll o d r =
  if (length o > length d) then
    positionAll o (concat [d, [getFirst r]]) (sortFromCenter ((pushOthers (getFirst r) (drop 1 r))))
  else d

distanceFromCenter : Room -> Float
distanceFromCenter r =
  sqrt ((w/u/2 - (fst r.c))^2 + (h/u/2 - (snd r.c))^2) 

centerComparison : Room -> Room -> Order
centerComparison r1 r2 =
    if ((distanceFromCenter r1) < (distanceFromCenter r2)) then
      LT
    else if ((distanceFromCenter r1) > (distanceFromCenter r2)) then
      GT
    else
      EQ

sortFromCenter : List Room -> List Room
sortFromCenter l =
    sortWith centerComparison l

----------------------------------------------------------------------------------------------
-- Step 3 : Select & Clean

isInGrid : Room -> Bool
isInGrid r =
  (fst r.c) >= 0 
  && (snd r.c) > 0 
  && ((fst r.c) + (toFloat r.w)) <= (w/u) 
  && ((snd r.c) + (toFloat r.h)) <= (h/u)

clean : List Room -> List Room
clean l =
    filter isInGrid l

isThisRoomARoom : Room -> Bool
isThisRoomARoom r = 
  if r.h >= 10 && r.w >= 10 then 
    True
  else
    False

selectRooms : List Room -> List Room
selectRooms l =
  List.map (\x -> if (isThisRoomARoom x) then changeColor (flagRoom x) purple else changeColor x white) l

----------------------------------------------------------------------------------------------
-- Step 4 : Path
distanceFromOrigin : Room -> Float
distanceFromOrigin r =
  sqrt ((fst r.c)^2 + (snd r.c)^2) 

originComparison : Room -> Room -> Order
originComparison r1 r2 =
    if ((distanceFromOrigin r1) < (distanceFromOrigin r2)) then
      LT
    else if ((distanceFromOrigin r1) > (distanceFromOrigin r2)) then
      GT
    else
      EQ

sortFromOrigin : List Room -> List Room
sortFromOrigin l =
    sortWith originComparison l

yAxisComparison : Room -> Room -> Order
yAxisComparison r1 r2 =
  if (snd r1.c < snd r2.c) then
    GT
  else if (snd r1.c > snd r2.c) then
    LT
  else
    EQ

sortFromYPosition : List Room -> List Room
sortFromYPosition l = 
  sortWith yAxisComparison l

onlyRealRoom : Room -> Bool
onlyRealRoom r = r.isRoom 

type alias Path = List (Float, Float)

getRoomCenter : Room -> (Float, Float)
getRoomCenter r = 
  (toFloat (round ((fst r.c) + (toFloat r.w)/2)), toFloat (round ((snd r.c) + (toFloat r.h)/2)))



fromTo : (Float, Float) -> (Float, Float) -> List (Float, Float) -> List (Float, Float)
fromTo p1 p2 acc =
  -- if (round (fst p1) == round (fst p2) && round (snd p1) == round (snd p2)) then 
  if (p1 == p2) then
    acc
  else
    -- go down
    if (snd p1 < snd p2) then
      fromTo (fst p1, (snd p1) + 1) p2 (concat [acc, [(fst p1, (snd p1) + 1)]])
    -- go up
    else if (snd p1 > snd p2) then
      fromTo (fst p1, (snd p1) - 1) p2 (concat [acc, [(fst p1, (snd p1) - 1)]])
    else 
      -- go right
      if (fst p1 < fst p2) then
        fromTo ((fst p1) + 1, snd p1) p2 (concat [acc, [((fst p1) + 1, snd p1)]])
      -- go left
      else 
        fromTo ((fst p1) - 1, snd p1) p2 (concat [acc, [((fst p1) - 1, snd p1)]])

createPath : Room -> Room -> Path
createPath r1 r2 = 
  fromTo (getRoomCenter r1) (getRoomCenter r2) [getRoomCenter r1, getRoomCenter r2]

createPaths : List Room -> List Path
createPaths l = 
  List.map2 createPath
    l (drop 1 l)

drawPath : Path -> Color -> List Form 
drawPath p c = 
  List.map (\x -> drawCell x c) p

drawPaths : List Path -> Color -> List Form
drawPaths l c =
  concat (List.map (\x -> drawPath x c) l)
----------------------------------------------------------------------------------------------
-- Grid Size
(w, h) = (800, 800)

-- Unit 
u = 8

-- Initial Seed
initSeed = 9

-- Step 1 : Create Random Rooms in a Circle
rad = 30.0
step1 = generateRooms 180 initSeed

-- Step 2 : Position Rooms
step2 = positionAll step1 [] (sortFromCenter (step1))

-- step 3 : Selectionner room / Clean
step3 = selectRooms (clean step2)

-- step 4 : Path 
step4 = createPaths (sortFromOrigin (List.filter onlyRealRoom step3))

-- step 6 : Creer les couloirs

-- step 7 : Ajouter texture

-- main
main : Html a         
main =  let one = Debug.log "test " step4 in toHtml <| 
    color lightGrey <| 
    collage w h <| 
    -- concat [gridLines, [], drawRooms step3, drawPaths step4 black]
    concat [[filled white <| rect 800 800], drawPaths step4 purple, drawRooms (List.filter onlyRealRoom step3)]

