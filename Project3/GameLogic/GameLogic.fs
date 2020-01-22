module GameLogic
type Game = int list
let move n i game =
    if i < (List.length game) || (List.item i game) < n then failwith (sprintf "move() failed with input n=%i i=%i game=%A" n i game)
    else List.mapi (fun i' x -> if i = i' then x-n else x) game |> List.filter (fun x -> x > 0)
let randomGame seed :Game = let rand = System.Random(seed)
                            [ 0 .. (rand.Next(7)+3)] |> List.map (fun _ -> rand.Next(4)+1)
let getGame (url:string) :Game = randomGame(int url)