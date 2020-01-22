module GameLogic
type Game = int list
let move n i game =
    if i < (List.length game) || (List.item i game) < n then failwith (sprintf "move() failed with input n=%i i=%i game=%A" n i game)
    else List.mapi (fun i' x -> if i = i' then x-n else x) game |> List.filter (fun x -> x > 0)
let randomGame seed :Game = [seed]
let getGame (url:string) :Game = [1]