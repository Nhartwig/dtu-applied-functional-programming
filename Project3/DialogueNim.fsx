// Prelude
open System
open System.Net
open System.Threading
open System.Windows.Forms
open System.Drawing

#r "GameLogic.dll"
open GameLogic
#r "AI.dll"
open AI
#r "GUI.dll"
//open GUI

// Game logic
type Player = Player | PC
type Game = int list
type Difficulty = Novice | Intermediate | Expert

// An asynchronous event queue kindly provided by Don Syme
type AsyncEventQueue<'T>() =
    let mutable cont = None
    let queue = System.Collections.Generic.Queue<'T>()
    let tryTrigger() =
        match queue.Count, cont with
        | _, None -> ()
        | 0, _ -> ()
        | _, Some d ->
            cont <- None
            d (queue.Dequeue())

    let tryListen(d) =
        if cont.IsSome then invalidOp "multicast not allowed"
        cont <- Some d
        tryTrigger()

    member x.Post msg = queue.Enqueue msg; tryTrigger()
    member x.Receive() =
        Async.FromContinuations (fun (cont,econt,ccont) ->
            tryListen cont)

// An enumeration of the possible events
type Message =    
    // Input
    | Start of Player
    | Choose of int * int
    | Restart
    | Random
    | Load of string
    | Cancel
    | Difficulty of Difficulty
    // Status
    | Move of int * int
    | Won of Player
    | GameReady of Game
    | Taunt of string
    | Turn of Player * Game
    | Error

// The dialouge automaton
let ev = AsyncEventQueue()
let rec init() =
    async {
        GUI.init()

        let! msg = ev.Receive()
        match msg with
        | Random   -> return! generatingRandom()
        | Load url -> return! getGame(url)
        | _ -> failwith (sprintf "init() failed, no match for: %A" msg)
    }
and generatingRandom() =
    async {

        GUI.generatingRandom()

        Async.StartWithContinuations
            (async {return randomGame 23},
             (fun game -> ev.Post (GameReady game)),
             (fun _ -> ev.Post(Error)),
             (fun _ -> ev.Post(Cancel)))
        
        let! msg = ev.Receive()
        match msg with
        | Cancel -> return! init()
        | GameReady game -> return! ready(game)
        | _ -> failwith (sprintf "generatingRandom() failed, no match for: %A" msg)
    }
and getGame(url) =
    async {

        GUI.getGame()

        Async.StartWithContinuations
            (async {return getOnlineGame url},
             (fun game -> ev.Post (GameReady game)),
             (fun _ -> ev.Post(Error)),
             (fun _ -> ev.Post(Cancel)))

        let! msg = ev.Receive()
        match msg with
        | Cancel -> return! init()
        | GameReady game -> return! ready(game)
        | _ -> failwith (sprintf "getGame() failed, no match for: %A" msg)
    }
and ready (game) =
    async {

        GUI.ready(game)

        let! msg = ev.Receive()
        match msg with
        | Start Player -> return! inProgressP(game)
        | Start PC     -> return! inProgressC(game)
        | Restart -> return! init()
        | _ -> failwith (sprintf "ready() failed, no match for: %A" msg)
    }
and inProgressC (game) =
    async {

        GUI.inProgressC(game)

        Async.StartWithContinuations
            (async {return chooseMove game},
             (fun (n, i) -> ev.Post (Choose (n,i))),
             (fun _ -> ev.Post(Error)),
             (fun _ -> ev.Post(Cancel)))

        let! msg = ev.Receive()
        match msg with
        | Choose (n,i) -> return! moving n i PC game
        | _ -> failwith (sprintf "inProgressC() failed, no match for: %A" msg)
    }
and inProgressP (game) =
    async {

        GUI.inProgressP(game)

        let! msg = ev.Receive()
        match msg with
        | Move (n,i) -> return! moving n i Player game
        | _ -> failwith (sprintf "inProgressP() failed, no match for: %A" msg)
    }
and moving n i player game =
    async {

        GUI.moving(game)

        use ts = new CancellationTokenSource()

        Async.StartWithContinuations
            (async { let game = move n i game
                     return game},
             (fun game -> match game with
                          | [] -> ev.Post(Won player)
                          | _  -> match player with
                                  | Player -> ev.Post(Turn (PC, game))
                                  | PC     -> ev.Post(Turn (Player, game))
                                  ),
             (fun _ -> ev.Post(Error)),
             (fun _ -> ev.Post(Cancel)),
             ts.Token)

        let! msg = ev.Receive()
        match msg with
        | Won p -> return! finish(p)
        | Turn (player, game) -> match player with
                                 | Player -> return! inProgressP game
                                 | PC     -> return! inProgressC game
        | _ -> failwith (sprintf "moving() failed, no match for: %A" msg)
    }
and finish (player) =
    async {

        GUI.finish(match player with
                   | Player -> GUI.Player
                   | PC -> GUI.PC)

        let! msg = ev.Receive()
        match msg with
        | Restart -> return! init()
        | _ -> failwith (sprintf "finish() failed, no match for: %A" msg)
    }
    
// Initialization
GUI.initialize()
GUI.randomGame (fun _ -> ev.Post Random)
GUI.loadGame (fun s -> ev.Post (Load s))
GUI.startGame (fun p _ -> match p with
                          | GUI.Player -> ev.Post (Start Player)
                          | GUI.PC -> ev.Post (Start PC))
GUI.chooseMove (fun (n,i) -> ev.Post (Move (n,i)))
GUI.cancel (fun _ -> ev.Post (Cancel))
GUI.restart (fun _ -> ev.Post (Restart))

// Start
Async.StartImmediate(init())
GUI.init()
GUI.show()