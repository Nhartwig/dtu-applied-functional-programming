module AI
type Game = int list
let chooseMove (game:Game) = (1,0)
let chooseMoveWithHandicap (handicap:int) (game:Game) = chooseMove(game)