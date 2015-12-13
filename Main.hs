import Random exposing (..)
import Dict exposing (Dict)
import Signal exposing (Address, message)
import StartApp.Simple as StartApp
import Color exposing(Color)
import Array
import String

import Html exposing (Html, div, fromElement, Attribute, input, span, text, toElement)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (..)
import Graphics.Collage exposing (Shape,Form,
  toForm,square,collage,rotate,moveY,filled,ngon,circle,text,rect,scale)
import Graphics.Element exposing (..)
import Graphics.Input exposing (clickable)
import Text exposing(fromString)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

randomSeed = 123457
rowN = 30
columnN = 30
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

type alias Model = {seed : Random.Seed, creatures : List Denizen, steps : Int, fishN : String, sharkN : String}

fish0 = Fish (Object 1 1 fishGestation fishStarvation)
fish1 = Fish (Object 2 3 fishGestation fishStarvation)
fish2 = Fish (Object 3 2 fishGestation fishStarvation)
fish3 = Fish (Object 3 4 fishGestation fishStarvation)
fish4 = Fish (Object 4 3 fishGestation fishStarvation)
fish5 = Fish (Object 3 3 fishGestation fishStarvation)
shark0 = Shark (Object 3 3 sharkGestation sharkStarvation)
shark1 = Shark (Object 3 3 -1 sharkStarvation)

initModel = Model (Random.initialSeed randomSeed) [shark0, fish0, fish1, fish2, fish3, fish4 ] 6 "5" "1"
model1 = Model (Random.initialSeed randomSeed) [fish5, fish0, fish1, fish2, fish3, fish4 ] 6 "6" "0"
model2 = Model (Random.initialSeed randomSeed) [shark1, fish0, fish1, fish2, fish3, fish4 ] 6 "5" "1"
model3 = Model (Random.initialSeed randomSeed) [shark1, fish0, fish1, fish2, fish3, fish4 ] 6 "2" "2"


type Action
   = MakeAMove
   | SetFishN String
   | SetShark String
   | Populate
   
update : Action -> Model -> Model               
update action model = 
   case action of
      MakeAMove -> updateWholeBoard model
      SetFishN s -> { model | fishN = s}
      SetShark s -> { model | sharkN = s}
      Populate -> populate (emptyCreatures model)

getInt : String -> Int
getInt s = case String.toInt s of 
  Ok n -> n
  Err _ -> 5

emptyCreatures : Model -> Model
emptyCreatures m  = { m | creatures = []}

populate : Model -> Model
populate  m = populateShark (getInt m.sharkN) (populateFish (getInt m.fishN)  (emptyCreatures m))  |> refresh

populateFish : Int -> Model -> Model
populateFish n m = case n of
  0 -> m
  _ -> let gen1 = Random.int 1 rowN in
          let (x, s1) = Random.generate gen1 m.seed in
          let (y, s2) = Random.generate gen1 s1 in
          case getCreature m.creatures (x, y) of
          Nothing -> populateFish (n-1) {m | creatures = m.creatures ++ [Fish (Object x y fishGestation fishStarvation)], seed = s2} 
          _ -> populateFish n { m | seed = s2} 
          
populateShark : Int -> Model -> Model
populateShark n m = case n of
  0 -> m
  _ -> let gen1 = Random.int 1 rowN in
          let (x, s1) = Random.generate gen1 m.seed in
          let (y, s2) = Random.generate gen1 s1 in
          case getCreature m.creatures (x, y) of
          Nothing -> populateShark (n-1) {m | creatures = m.creatures ++ [Shark (Object x y sharkGestation sharkStarvation)], seed = s2} 
          _ -> populateShark n { m | seed = s2} 

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


gen = Random.int 1 4

updateSingle : Model -> Model
updateSingle m = case m.creatures of 
  [] -> m
  (x :: xs) -> if snd (getTimes x) <= 1 then { m | creatures = xs, steps = m.steps - 2}
  else  let     (i1, s1) = Random.generate gen m.seed in
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
  _ -> { m | creatures = List.filter (\d -> getCor d /= (a, b)) m.creatures, steps = m.steps - 1} |> refillShark
  
refillShark : Model -> Model
refillShark m = 
 case m.creatures of 
 [] -> m
 (x :: xs) ->
 let cor = getCor x in
 let times = getTimes x in
 { m | creatures = (newCreature x (fst cor) (snd cor) (fst times) sharkStarvation) :: xs}

makeMove : Model -> (Int, Int) -> Model
makeMove m (a, b) = 
  case m.creatures of 
  [] -> m
  (x :: xs) -> let times = getTimes x in 
                   let n = (newCreature x a b (fst times - 1) (snd times - 1)) in
                   let newMum = (newNewMum x a b  (snd times - 1)) in
                   if fst times <= 1 then { m | creatures = xs ++ [newMum, (newBaby x (getCor x))]}
                   else                             { m | creatures = xs ++ [n]}
  
  
newCreature : Denizen -> Int -> Int -> Int -> Int -> Denizen
newCreature de a b c d = let object =  (Object a b c d) in case de of
  Fish _ -> (Fish object)
  Shark _ -> (Shark object)
  
newNewMum : Denizen -> Int -> Int -> Int -> Denizen
newNewMum de a b d = case de of 
  Fish _ -> Fish (Object a b fishGestation d)
  Shark _ -> Shark (Object a b sharkGestation d)

newBaby : Denizen -> (Int, Int) -> Denizen
newBaby de (a, b)= case de of 
  Fish _ -> newCreature de a b fishGestation fishStarvation
  Shark _ -> newCreature de a b sharkGestation sharkStarvation

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

--main = show (populate model3)
                        
-------------TEST--------------------
--main = flow down <| List.map showTestResult
--       [testUpdateNTImes, testRefresh, testRemoveCreature1, testRemoveCreature2, testNewPosition
--       , testCanMove, testMakeMove, testMakeMove1, testGetCreature1, testGetCreature2, testGetCreature3]
       
testUpdateNTImes : Test
testUpdateNTImes = let
  t = Test ("testUpdateNTImes") Pass
  m = initModel
  newM = updateNTimes m
  in
  if newM.steps == 0 then t
  else { t | result  = Fail <| flow right [show newM] }
  
testRefresh : Test
testRefresh = let
  t = Test ("testRefresh") Pass
  m = initModel
  newM = refresh (updateNTimes m)
  in
  if newM.steps == 5 then t
  else { t | result  = Fail <| flow right [show newM] }
  
testRemoveCreature1 : Test
testRemoveCreature1 = let
  t = Test ("testRemoveCreature1") Pass
  m = initModel
  newM = removeCreature m (2, 3)
  in
  if List.length newM.creatures == 5 then t
  else { t | result  = Fail <| flow right [show newM] }
  
testRemoveCreature2 : Test
testRemoveCreature2 = let
  t = Test ("testRemoveCreature2") Pass
  m = initModel
  newM = removeCreature m (10, 3)
  in
  if List.length newM.creatures == 6 then t
  else { t | result  = Fail <| flow right [show newM] }

testNewPosition : Test
testNewPosition = let
  t = Test ("testNewPosition") Pass
  p1 = newPosition UP (2, 3)
  p2 = newPosition DOWN (rowN, 3)
  in
  if p1 == (1, 3) && p2 == (1, 3) then t
  else { t | result  = Fail <| flow right [show p1, show p2] }
  
testCanMove : Test
testCanMove = let
  t = Test ("testCanMove") Pass
  result1 = canMove initModel.creatures UP
  result2 = canMove model1.creatures UP
  result3 = canMove model1.creatures DOWN
  in
  if result1 == True && result2 == False && result3 == False then t
  else { t | result  = Fail <| flow right [show result1, show result2, show result3] }
    
testMakeMove : Test
testMakeMove = let
  t = Test ("testMakeMove") Pass
  result1 = makeMove initModel (4, 3)
  in
  if  case List.head (List.reverse result1.creatures) of 
  Nothing -> True
  Just a -> getTimes a == (sharkGestation - 1, sharkStarvation - 1) then t
  else { t | result  = Fail <| flow right [show result1] }
  
testMakeMove1: Test
testMakeMove1 = let
  t = Test ("testMakeMove1") Pass
  result1 = makeMove model2 (4, 3)
  in
  if  case List.head (List.reverse result1.creatures) of 
  Nothing -> True
  Just a -> getTimes a == (sharkGestation, sharkStarvation) then t
  else { t | result  = Fail <| flow right [show result1] }
  
testGetCreature1: Test
testGetCreature1 = let
  t = Test ("testGetCreature1") Pass
  result1 = getCreature initModel.creatures (1, 1) in
  if  case result1 of
  Just (Fish _) -> True
  _ -> False then t
  else { t | result  = Fail <| flow right [show result1] }
  
testGetCreature2: Test
testGetCreature2 = let
  t = Test ("testGetCreature2") Pass
  result1 = getCreature initModel.creatures (3, 3) in
  if  case result1 of
  Just (Shark _) -> True
  _ -> False then t
  else { t | result  = Fail <| flow right [show result1] }
  
testGetCreature3: Test
testGetCreature3 = let
  t = Test ("testGetCreature3") Pass
  result1 = getCreature initModel.creatures (10, 3) in
  if  case result1 of
  Nothing  -> True
  _ -> False then t
  else { t | result  = Fail <| flow right [show result1] }
 
type TestResult = Fail Element | Pass
showTestResult t = flow right
  (container 150 30 middle (leftAligned (fromString t.name)) ::
   spacer 10 10 :: 
  case t.result of
    Fail e -> [color Color.red   (spacer 20 20), spacer 10 10, e]
               
    Pass   -> [color Color.green (spacer 20 20)])
              

type alias Test = { name : String, result : TestResult }

------------Test End------------------------

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
         
choiceButton : Address Action -> Model -> String -> Element
choiceButton address model s =
    if s == "Populate" then Graphics.Input.button (Signal.message address Populate) s
    else Graphics.Input.button (Signal.message address MakeAMove) s

field : String -> Address Action -> (String -> Action) -> String -> String -> Html
field fieldType address toAction name content =
  div []
    [ div [fieldNameStyle "160px"] [Html.text name]
    , input
        [ type' fieldType
        , placeholder name
        , value content
        , on "input" targetValue (\string -> Signal.message address (toAction string))
        ]
        []
    ]


fieldNameStyle : String -> Attribute
fieldNameStyle px =
  style
    [ ("width", px)
    , ("padding", "10px")
    , ("text-align", "right")
    , ("display", "inline-block")
    ]

view : Address Action -> Model -> Html  
view address model = 
    div[] [fromElement (showBoard model `beside`
              (container panelWidth boardSize middle
                (flow up [choiceButton address model "Make a Move",
                          choiceButton address model "Populate"]))),
                field "text" address SetFishN "Set Fish Number" model.fishN,
                field "text" address SetShark "Set Shark Number" model.sharkN]











