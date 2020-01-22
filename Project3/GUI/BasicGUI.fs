module GUI

open System.Windows.Forms 
open System.Drawing 

type Player = Player | PC
type Game = int list
type Difficulty = Novice | Intermediate | Expert

// The window part
let window =
  new Form(Text="Nim", Size=Size(500,200))

let randomButton =
    new Button(Location=Point(10,10),Size=Size(235,25), Text="Random")

let loadButton =
    new Button(Location=Point(245,10),Size=Size(235,25), Text= "Load")

let startPlayer =
    new Button(Location=Point(10, 40),Size=Size(235,25), Text= "Start Player")
   
let startPC =
    new Button(Location=Point(245,40), Size=Size(235,25), Text= "Start PC")

let gameButton =
    new Button(Location=Point(10,70), Size=Size(480, 25), Text="Game Button")

let buttons = [randomButton;loadButton; startPlayer; startPC; gameButton]

let disable bs = 
    for b in buttons do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false

let enable bs =
    for b in buttons do
        b.Enabled <- false
    for (b:Button) in bs do
        b.Enabled <- true

let disableAll() =
    for b in buttons do
        b.Enabled <- false

// Initialization
let initialize() = 
    for b in buttons do
        window.Controls.Add b
    
let randomGame (f: (unit -> unit)) = randomButton.Click.Add (fun _ -> f())
let loadGame (f: (string -> unit)) = loadButton.Click.Add (fun _ -> f("www.google.dk"))
let startGame (f: (Player -> Difficulty -> unit)) = startPlayer.Click.Add (fun _ -> f Player Expert)
                                                    startPC.Click.Add (fun _ -> f PC Expert)
let chooseMove (f: (int * int -> unit)) = gameButton.Click.Add (fun _ -> f (1,0))
let cancel (f: (unit -> unit)) = ()
let restart (f: (unit -> unit)) = ()

// Show GUI
let show() = window.Show()

// Interactions
let taunt (s: string) = ()

// State changes
let init() = enable [randomButton; loadButton]
let generatingRandom() = disableAll()
let getGame() = disableAll()
let ready (game: Game) = enable [startPlayer; startPC]
let inProgressC (game: Game) = gameButton.Text <- (sprintf "%A" game)
                               disableAll()
let inProgressP (game: Game) = gameButton.Text <- (sprintf "%A" game)
                               enable [gameButton]
let moving (game: Game) = disableAll()
let finish (player: Player) = 
    let msg = 
        match player with
        | Player -> "Player won!"
        | PC     -> "PC won!"
    MessageBox.Show(msg) |> ignore
