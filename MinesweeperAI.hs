type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState
