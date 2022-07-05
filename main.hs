type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up:: MyState -> MyState

up (S (curx,cury) mines action prev) = if curx==0 then Null else (S (curx-1,cury) mines "up" (S (curx,cury) mines action prev))



down:: MyState -> MyState

down (S (curx,cury) mines action prev) = if curx==3 then Null else (S (curx+1,cury) mines "down" (S (curx,cury) mines action prev))



left:: MyState -> MyState

left (S (curx,cury) mines action prev) = if cury==0 then Null else (S (curx,cury-1) mines "left" (S (curx,cury) mines action prev))



right:: MyState -> MyState

right (S (curx,cury) mines action prev) = if cury==3 then Null else (S (curx,cury+1) mines "right" (S (curx,cury) mines action prev))



collect:: MyState -> MyState

collect (S (curx,cury) mines action prev) = helper_collect (S (curx,cury) mines action prev) [] mines



helper_collect :: MyState -> [(Int,Int)] -> [(Int,Int)] -> MyState

helper_collect (S _ [] _ _) _ _= Null
helper_collect (S (curx,cury) ((minex,miney):t) action prev) acc mines = if curx==minex && cury==miney then (S (curx,cury) (acc++t) "collect" (S (curx,cury) mines action prev)) else helper_collect (S (curx,cury) t action prev) ([(minex,miney)]++acc) mines



nextMyStates :: MyState -> [MyState]

nextMyStates state = filter2 Null (get_all state)



get_all:: MyState -> [MyState]

get_all state = [up state, down state, left state, right state, collect state]



filter2:: MyState -> [MyState] -> [MyState]

filter2 _ [] = []
filter2 x (h:t) = if x==h then filter2 x t else h:(filter2 x t)



isGoal:: MyState -> Bool

isGoal (S _ [] _ _) = True
isGoal (S _ _ _ _) = False



search:: [MyState] -> MyState

search (h:t) = if isGoal h then h else search (t++(nextMyStates h))



constructSolution:: MyState -> [String]

constructSolution (S _ _ _ Null) = []
constructSolution (S location mines action prev) = (constructSolution prev) ++ [action]



solve :: Cell -> [Cell] -> [String]

solve location mines = constructSolution (search (nextMyStates (S location mines "" Null)))