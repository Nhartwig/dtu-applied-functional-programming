module AI
type Game = int list
let chooseMove (game:Game) = 
    let m = List.fold (^^^) 0 game
    if m = 0
    then (1,List.findIndex (fun x -> x = List.max game) game)
    else List.findIndex (fun x -> (x ^^^ m) < x) game |> fun i -> (List.item i game - (List.item i game ^^^ m),i)
let chooseMoveWithHandicap (handicap:int) (game:Game) = chooseMove(game)