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

let disable bs = 
    for b in [randomButton;loadButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false

// Initialization
let initialize() = 
    window.Controls.Add randomButton
    window.Controls.Add loadButton
    
let randomGame (f: (unit -> unit)) = randomButton.Click.Add (fun _ -> f())
let loadGame (f: (string -> unit)) = loadButton.Click.Add (fun _ -> f("www.google.dk"))
let startGame (f: (Player -> Difficulty -> unit)) = ()
let chooseMove (f: (int * int -> unit)) = ()
let cancel (f: (unit -> unit)) = ()
let restart (f: (unit -> unit)) = ()

// Show GUI
let show() = window.Show()

// Interactions
let taunt (s: string) = ()

// State changes
let init() = ()
let generatingRandom() = ()
let getGame() =()
let ready (game: Game) = ()
let inProgressC (game: Game) = ()
let inProgressP (game: Game) = ()
let moving (game: Game) = ()
let finish (player: Player) = ()
