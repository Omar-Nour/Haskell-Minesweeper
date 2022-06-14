--NxM implementation
type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState Int Int deriving (Show, Eq)

up:: MyState -> MyState
up Null = Null
up (S (y,x) mines prevAction state n m) | y <= 0 = Null
                                         | otherwise = (S (y-1,x) mines "up" (S (y,x) mines prevAction state n m) n m)

down:: MyState -> MyState
down Null = Null
down (S (y,x) mines prevAction state n m) | y >= n = Null
                                          | otherwise = (S (y+1,x) mines "down" (S (y,x) mines prevAction state n m) n m)

left:: MyState -> MyState
left Null = Null
left (S (y,x) mines prevAction state n m) | x <= 0 = Null
                                          | otherwise = (S (y,x-1) mines "left" (S (y,x) mines prevAction state n m) n m)

right:: MyState -> MyState
right Null = Null
right (S (y,x) mines prevAction state n m) | x >= m = Null
                                           | otherwise = (S (y,x+1) mines "right" (S (y,x) mines prevAction state n m) n m)

-- delete Null x = x
delete y (h:t) | h == y = t
               | otherwise = [h] ++ delete y t

collect:: MyState -> MyState
collect Null = Null
collect (S curr mines prevAction state n m) | elem curr mines = (S curr (delete curr mines) "collect" (S curr mines prevAction state n m) n m)
                                        | otherwise = Null


nextMyStates::MyState->[MyState]
nextMyStates (S curr mines prevAction state n m) =   filter (/=Null) [(up (S curr mines prevAction state n m)), (left (S curr mines prevAction state n m)), (right (S curr mines prevAction state n m)), (down (S curr mines prevAction state n m)),(collect (S curr mines prevAction state n m))]

isGoal::MyState->Bool
isGoal Null = False
isGoal (S curr mines prevAction state n m) =  mines == []

search::[MyState]->MyState
search [] = Null
search (h:t) | isGoal h = h
             | otherwise = search (t++(nextMyStates h))

constructSolution:: MyState ->[String]
constructSolution Null = []
constructSolution (S curr mines prevAction state n m) =  filter (/="") ((constructSolution state)++[prevAction])

maxX [(y,x)] = x
maxX ((y,x):t) | (maxX t) > x = maxX t
               | otherwise = x

maxY [(y,x)] = y
maxY ((y,x):t) | (maxY t) > y = maxY t
               | otherwise = y

solve :: Cell->[Cell]->[String]
solve (y,x) (h:t) = constructSolution (search([(S (y,x) (h:t) "" Null (maxY (   [(y,x)]++(h:t)  ))   (   maxX ([(y,x)]++(h:t))    ) )]))
