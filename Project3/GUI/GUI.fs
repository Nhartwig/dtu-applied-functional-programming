module GUI

// Prelude
open System 
open System.Net 
open System.Threading 
open System.Windows.Forms
open System.Drawing

type Player = Player | PC
type Game = int list
type Difficulty = Novice | Intermediate | Expert
type ID = int


type button = 
    | NewGame of Button 
    | Random of Button 
    | Seed of Button
    | Quit of Button

type box = 
    | Welcome of string*MessageBox
    | GameBox of int*int*Boolean*GroupBox
    | Match of ID*int*int*Boolean*Button

type window = 
    | MainWindow of int*int*Form

// user wins = true, user loses = false
type popup = 
    | Taunt of string*Form
    | Outcome of Boolean*Form





// Match functions (generate matches, pick/remove/update matches)
let createMatches = //
let updateMatches = //






let createSelDifficultyRadios = 
    let rb1 = new RadioButton((Name="Novice",Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Novice"))
    let rb2 = new RadioButton((Name="Intermediate",Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Intermediate"))
    let rb3 = new RadioButton((Name="Expert",Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Expert"))
    [rb1; rb2; rb3]



let createSelectPlayerRadios = 
    let playerFirst = new RadioButton((Name="player" ,Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Go First"))
    let aiFirst = new RadioButton((Name="ai", Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Let AI Go First"))
    [pFirst; aiFirst]


let createButtons = 
    let gamebutton = new NewGame(Location=Point(100,1),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="New Game!")
    let randombutton = new Random(Location=Point(100,1),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Random Game")
    let seedbutton = new Seed(Location=Point(100,1),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Seed Game from Web")
    let quitButton = new Quit(Location=Point(100,1),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="New Game!")
    [gamebutton; randombutton; seedbutton; quitButton]

let createWindow = 
    let window = new MainWindow(Text="Nim Game", Size=Size(800,800))
    window

let buttonList = createButtons

let addButtons window buttons = List.fold (fun btn window -> window.Controls.Add btn) window buttons

let controlsMap = Map.ofList (List.zip ["gameButton"; "randomButton";"seedButton"; "startPlayer";"quitButton"] (buttonList))

let selPlayerRadios = createSelectPlayerRadios
let selDifficultyRadios = createSelDifficultyRadios 




let getRadioSelection (radios:RadioButton list) : string = 
    let (res:RadioButton) = (List.tryFind (radio.Checked = true) radios)
    res.Name

// Initialization

let window = createWindow

let initialize() = 
    (window buttonList |> addButtons)

    window.Controls.Add (selPlayerBox)
    window.Controls.Add (selDifficultyBox)
    controlsMap.Add ("selPlayerBox", selPlayerBox)
    controlsMap.Add ("selDifficultyBox", selDifficultyBox)
    ()


val randomGame : (unit -> unit) -> unit
let randomGame (func: (unit-> unit)) = (controlsMap.TryFind "randomButton").Click.Add (fun _ -> func())

val loadGame : (string -> unit) -> unit
let loadGame (func:(str-> unit)) = (controlsMap.TryFind "loadButton").Click.Add (fun str -> func(str)) 

val startGame : (Player -> Difficulty -> unit) -> unit
let startGame (func: (Player -> Difficulty -> unit)) = 

val chooseMove : (int * int -> unit) -> unit
val cancel : (unit -> unit) -> unit
val restart : (unit -> unit) -> unit

// Show GUI

let show() = Application.Run(window)

// Interactions
let taunt str = 
    match str with
    | str ->    let popup = new Form(Text=str, Size=Size(300,300))
                Application.Run(popup)

// State changes
val init : unit -> unit
val generatingRandom : unit -> unit
val getGame : unit -> unit
val ready : Game -> unit
val inProgressC : Game -> unit
val inProgressP : Game -> unit
val moving : Game -> unit
val finish : Player -> unit






