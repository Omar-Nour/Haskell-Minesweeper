-- 4X4 Implementation
type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)


up:: MyState -> MyState
up Null = Null
up (S (y,x) mines prevAction state) | y <= 0 = Null
                                    | otherwise = (S (y-1,x) mines "up" ((S (y,x) mines prevAction state)))

down:: MyState -> MyState
down Null = Null
down (S (y,x) mines prevAction state) | y >= 3 = Null
                                      | otherwise = (S (y+1,x) mines "down" (S (y,x) mines prevAction state))

left:: MyState -> MyState
left Null = Null
left (S (y,x) mines prevAction state) | x <= 0 = Null
                                      | otherwise = (S (y,x-1) mines "left" ((S (y,x) mines prevAction state)))

right:: MyState -> MyState
right Null = Null
right (S (y,x) mines prevAction state) | x >= 3 = Null
                                       | otherwise = (S (y,x+1) mines "right" ((S (y,x) mines prevAction state)))

-- delete Null x = x
delete y (h:t) | h == y = t
               | otherwise = [h] ++ delete y t

collect:: MyState -> MyState
collect Null = Null
collect (S curr mines prevAction state) | elem curr mines = (S curr (delete curr mines) "collect" (S curr mines prevAction state))
                                        | otherwise = Null


nextMyStates::MyState->[MyState]
--nextStates Null = Null
nextMyStates (S curr mines prevAction state) =   filter (/=Null) [(up (S curr mines prevAction state)), (left (S curr mines prevAction state)), (right (S curr mines prevAction state)), (down (S curr mines prevAction state))]

isGoal::MyState->Bool
isGoal Null = False
isGoal (S curr mines prevAction state) =  mines == []
