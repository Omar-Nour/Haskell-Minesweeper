-- 4X4 Implementation
type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState


up:: MyState -> MyState
up Null = Null
up (S (y,x) mines prevAction MyState) | y <= 0 = Null
                                      | otherwise = (S (y-1,x) mines "up" ((S (y,x) mines prevAction MyState)))

down:: MyState -> MyState
down Null = Null
down (S (y,x) mines prevAction MyState) | y >= 3 = Null
                                        | otherwise = (S (y+1,x) mines "down" (S (y,x) mines prevAction MyState))

left:: MyState -> MyState
left Null = Null
left (S (y,x) mines prevAction MyState) | x <= 0 = Null
                                        | otherwise = (S (y,x-1) mines "left" ((S (y,x) mines prevAction MyState)))

right:: MyState -> MyState
right Null = Null
right (S (y,x) mines prevAction MyState) | x >= 3 = Null
                                         | otherwise = (S (y,x+1) mines "right" ((S (y,x) mines prevAction MyState)))
