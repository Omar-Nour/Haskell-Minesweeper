--NxM implementation
type Cell = (Int,Int)
--the two new ints are for the bounds of the grid
data MyState = Null | S Cell [Cell] String MyState Int Int deriving (Show, Eq)

--added grid bounds in type definitions
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

delete y (h:t) | h == y = t
               | otherwise = [h] ++ delete y t

collect:: MyState -> MyState
collect Null = Null
collect (S curr mines prevAction state n m) | elem curr mines = (S curr (delete curr mines) "collect" (S curr mines prevAction state n m) n m)
                                        | otherwise = Null


nextMyStates::MyState->[MyState]
nextMyStates (S curr mines prevAction state n m) =   filter (/=Null) [(up (S curr mines prevAction state n m)), (left (S curr mines prevAction state n m)), (right (S curr mines prevAction state n m)), (down (S curr mines prevAction state n m)),(collect (S curr mines prevAction state n m))]


manhattan (y,x) (y2,x2) = abs(y-y2)+abs(x-x2)

-- they work
closest (S (y,x) ((yMine,xMine):t) prevAction state n m) = closestMine (S (y,x) ((yMine,xMine):t) prevAction state n m) (n*m) (n,m)
closestMine (S (y,x) [] prevAction state n m) minDist minMine = minMine
closestMine (S (y,x) ((yMine,xMine):t) prevAction state n m) minDist minMine
  | manhattan (y,x) (yMine,xMine) < minDist = closestMine (S (y,x) t prevAction state n m) (manhattan (y,x) (yMine,xMine)) (yMine,xMine)
  | otherwise = closestMine (S (y,x) t prevAction state n m) minDist minMine

--closestState closest_mine ((S curr mines prevAction state n m):t) = closest

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

-- infer size of grid from initial position and mine positions
solve :: Cell->[Cell]->[String]
solve (y,x) (h:t) = constructSolution (search([(S (y,x) (h:t) "" Null (maxY ([(y,x)]++(h:t))) (maxX ([(y,x)]++(h:t))))]))
