import Random exposing (..)
import Dict exposing (Dict)
import Signal exposing (Address, message)
import StartApp.Simple as StartApp
import Color exposing(Color)

import Html exposing (Html, div, fromElement)
import Html.Events exposing (onClick)
import Graphics.Collage exposing (Shape,Form,
  toForm,square,collage,rotate,moveY,filled,ngon,circle,text,rect,scale)
import Graphics.Element exposing (..)
import Graphics.Input exposing (clickable)
import Text exposing(fromString)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

randomSeed = 123456
rowN = 75
columnN = 75
fishStarvation = 10000
fishGestation = 10
sharkStarvation = 20
sharkGestation = 20


type alias Object = { row : Int, column : Int, timeToGestation : Int, timeToStarvation : Int }

single : Object
single = Object 0 0 5 5

type Denizen 
    = Fish Object
    | Shark Object
    
getCor : Denizen -> (Int, Int)
getCor d = 
  case d of 
  Fish f -> (f.row, f.column)
  Shark s -> (s.row, s.column)
  
getTimes : Denizen -> (Int, Int)
getTimes d = 
  case d of 
  Fish f -> (f.timeToGestation, f.timeToStarvation)
  Shark s -> (s.timeToGestation, s.timeToStarvation)

type Direction = LEFT | RIGHT | UP | DOWN

type alias Model = {seed : Random.Seed, creatures : List Denizen, steps : Int}

fish0 = Fish (Object 1 1 fishGestation fishStarvation)
fish1 = Fish (Object 2 3 fishGestation fishStarvation)
fish2 = Fish (Object 3 2 fishGestation fishStarvation)
fish3 = Fish (Object 3 4 fishGestation fishStarvation)
fish4 = Fish (Object 4 3 fishGestation fishStarvation)
shark0 = Shark (Object 3 3 sharkGestation sharkStarvation)

initModel = Model (Random.initialSeed randomSeed) [shark0, fish0, fish1, fish2, fish3, fish4 ] 6

type Action
   = MakeAMove
   
update : Action -> Model -> Model               
update action model = 
   case action of
      MakeAMove -> updateWholeBoard model
      
refresh: Model -> Model
refresh m = { m | steps = List.length m.creatures}

updateWholeBoard : Model -> Model
updateWholeBoard m = updateNTimes m |> refresh

updateNTimes : Model -> Model
updateNTimes m  = 
  case m.steps of 
  0 -> m
  _ -> updateSingle m |> updateNTimes
  
getDirection : Int -> Direction
getDirection n = case n of
  1 -> LEFT
  2 -> RIGHT
  3 -> UP
  _ -> DOWN

updateSingle : Model -> Model
updateSingle m = case m.creatures of 
  [] -> m
  (x :: xs) -> if snd (getTimes x) <= 1 then { m | creatures = xs, steps = m.steps - 2}
  else  let         gen  = Random.int 1 4  in
          let     (i1, s1) = Random.generate gen m.seed in
          let    d = getDirection i1 in
          let    newM = { m | seed = s1, steps = m.steps - 1} in
     if canMove newM.creatures d then 
       let newP = newPosition d (getCor x) in 
       makeMove (removeCreature newM newP) newP
     else let times = getTimes x in
            let cor = getCor x in
     { newM | creatures = xs ++ [newCreature x (fst cor) (snd cor) (fst times - 1) (snd times - 1)]}
     
removeCreature : Model -> (Int, Int) -> Model
removeCreature m (a, b) = 
  let l = List.filter (\d -> getCor d == (a, b)) m.creatures in 
  case l of 
  [] -> m
  _ -> { m | creatures = List.filter (\d -> getCor d /= (a, b)) m.creatures, steps = m.steps - 1}

makeMove : Model -> (Int, Int) -> Model
makeMove m (a, b) = 
  case m.creatures of 
  [] -> m
  (x :: xs) -> let times = getTimes x in 
                   let n = (newCreature x a b (fst times - 1) (snd times - 1)) in
                   if fst times <= 1 then { m | creatures = xs ++ [n, (newBaby x (getCor x))]}
                   else                             { m | creatures = xs ++ [n]}
  
  
newCreature : Denizen -> Int -> Int -> Int -> Int -> Denizen
newCreature de a b c d = let object =  (Object a b c d) in case de of
  Fish _ -> (Fish object)
  Shark _ -> (Shark object)

newBaby : Denizen -> (Int, Int) -> Denizen
newBaby de (a, b)= case de of 
  Fish _ -> newCreature de a b fishGestation fishStarvation
  Shark _ -> newCreature de a b fishGestation sharkStarvation

--[{ x | timeToGestation = x.timeToGestation - 1, timeToStarvation =x.timeToStarvation - 1}]
newPosition : Direction -> (Int, Int) -> (Int, Int)
newPosition d (x, y) = 
     case d of 
     LEFT -> (x, (y - 1 + columnN) % columnN)
     RIGHT -> (x, (y + 1 + columnN) % columnN)
     UP -> ((x - 1 + rowN) % rowN, y)
     DOWN -> ((x + 1 + rowN) % rowN, y)
     
canMove : List Denizen -> Direction -> Bool
canMove list d = 
     case list of 
     [] -> False
     (x :: xs) -> let nearC = (getCreature (x :: xs) (newPosition d (getCor x))) in case x of 
     Fish _ -> case nearC of
       Nothing -> True
       _ -> False
     Shark _ -> case nearC of 
       Nothing -> True
       Just (Fish _) -> True
       Just (Shark _) -> False

--
getCreature : List Denizen -> (Int, Int) -> Maybe Denizen

getCreature list (x, y) = case List.filter (\d -> getCor d == (x, y)) list of
   [] -> Nothing
   (x :: xs) -> Just x


main = StartApp.start { model  = initModel, 
                        view   = view, 
                        update = update }

------------VIEW---------------------------
scaleFactor = 1

tileSize   = 14 * scaleFactor  
squareSize = 12 * scaleFactor

boardSize  = rowN * tileSize  

panelWidth  = 170

showBoard : Model -> Element           
showBoard m = List.map (showColumn m) [1 .. columnN]
                |> flow right
                
showColumn : Model -> Int -> Element
showColumn m i =
  let draw j = case getCreature m.creatures (j, i) of
                Nothing -> layers [box]
                Just (Fish _) -> layers [fishBox]
                Just (Shark _) -> layers [sharkBox]
      ranks  = List.map draw [1 ..  rowN]
  in
  flow down (ranks)
       |> container tileSize boardSize middle
       
toTile : Form -> Element
toTile x = collage tileSize tileSize [x]
       
box : Element           
box = square squareSize
         |> filled (Color.rgb 230 230 250)
         |> toTile
         
fishBox : Element           
fishBox = square squareSize
         |> filled (Color.blue)
         |> toTile
         
sharkBox : Element           
sharkBox = square squareSize
         |> filled (Color.red)
         |> toTile
         
-- create buttons for the move choices
choiceButton : Address Action -> Model -> Element
choiceButton address model =
    Graphics.Input.button (Signal.message address MakeAMove) "MakeAMove"

view : Address Action -> Model -> Html  
view address model = 
    div[] [fromElement (showBoard model `beside`
              (container panelWidth boardSize middle
                (flow up [choiceButton address model])))]












