module AI
type Game = int list
val chooseMove : Game -> int * int
val chooseMoveWithHandicap : int -> Game -> int * int