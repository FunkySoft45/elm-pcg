module Steps exposing (..)

import List exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Maybe exposing (..)
import Random exposing (..)


import Room exposing (..)
import Grid exposing (..)

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
-----------------------------------------------------------------------------------------
-- Step 1 : Create Random Rooms in a Circle
rad = 30.0

goStep1 : Int -> List Room
goStep1 seed =
  generateRooms 180 seed

drawStep1 : Int -> List Form
drawStep1 seed =
  concat [gridLines, drawRooms (goStep1 seed)]

-- Step 2 : Position Rooms

goStep2 : Int -> List Room
goStep2 seed =
  positionAll (goStep1 seed) [] (sortFromCenter (goStep1 seed))

drawStep2 : Int -> List Form
drawStep2 seed =
  concat [gridLines, drawRooms (goStep2 seed)]


-- step 3 : Selectionner room / Clean

goStep3 : Int -> List Room
goStep3 seed =
  selectRooms (clean <| goStep2 seed)

drawStep3 : Int -> List Form
drawStep3 seed =
  concat [gridLines, drawRooms (goStep3 seed)]
         

-- step 4 : Path 

goStep4 : Int -> List Path
goStep4 seed =
  createPaths (sortFromOrigin (List.filter onlyRealRoom (goStep3 seed)))

drawStep4 : Int -> List Form
drawStep4 seed =
  concat [ gridLines, drawRooms (goStep3 seed), drawPaths (goStep4 seed) black ]

drawStep5 : Int -> List Form
drawStep5 seed =            
  concat [ drawRooms (goStep3 seed), drawPaths (goStep4 seed) purple, gridLines ]
