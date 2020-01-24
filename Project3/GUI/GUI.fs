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
let createMatches = ()

let updateMatches = ()

(*let createDifficultyRadios = 
    let rb1 = new RadioButton(Name="Novice",Location=Point(100,800),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Novice")
    let rb2 = new RadioButton(Name="Intermediate",Location=Point(100,800),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Intermediate")
    let rb3 = new RadioButton(Name="Expert",Location=Point(100,800),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Expert")
    [rb1; rb2; rb3]*)

let createDifficultyRadios = 
    let rb1 = new RadioButton(Name="Novice",MinimumSize=Size(10,10),MaximumSize=Size(100,30),Text="Novice")
    let rb2 = new RadioButton(Name="Intermediate",MinimumSize=Size(10,10),MaximumSize=Size(100,30),Text="Intermediate")
    let rb3 = new RadioButton(Name="Expert",MinimumSize=Size(10,10),MaximumSize=Size(100,30),Text="Expert")
    [rb1; rb2; rb3]

(*let createPlayerRadios = 
    let playerFirst = new RadioButton(Name="player" ,Location=Point(500,800),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Go First")
    let aiFirst = new RadioButton(Name="ai", Location=Point(500,800),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Let AI Go First")
    [playerFirst; aiFirst]*)

let createPlayerRadios = 
    let playerFirst = new RadioButton(Name="player",MinimumSize=Size(10,10),MaximumSize=Size(100,30),Text="Go First")
    let aiFirst = new RadioButton(Name="ai",MinimumSize=Size(10,10),MaximumSize=Size(100,30),Text="AI goes First")
    [playerFirst; aiFirst]

(*let createButtons = 
    let gameButton = new Button(Name="gameButton",Location=Point(100,300),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Game button")
    let randomButton = new Button(Name="randomButton",Location=Point(200,300),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Randomized")
    let seedButton = new Button(Name="seedButton",Location=Point(300,300),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Webpage-Seeded")
    let startButton = new Button(Name="startButton",Location=Point(400,300),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Start Game!")
    let quitButton = new Button(Name="quitButton", Location=Point(500,300),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Quit Game ;(")
    [gameButton; randomButton; seedButton; startButton; quitButton]*)

let createButtons = 
    let gameButton = new Button(Name="gameButton",MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Game button")
    let randomButton = new Button(Name="randomButton",MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Randomized")
    let seedButton = new Button(Name="seedButton",MinimumSize=Size(150,50),MaximumSize=Size(150,50),Text="Webpage-Seeded")
    let startButton = new Button(Name="startButton",MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Start Game!")
    let quitButton = new Button(Name="quitButton",MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Quit Game ;(")
    let restartButton = new Button(Name="restartButton",MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Restart Game")
    [gameButton; randomButton; seedButton; startButton; quitButton; restartButton]

let createWindow str x y = 
    match str, x, y with
    | str, x, y when (((x > 0) && (y > 0)) && ((String.length (str)) > 0))-> new Form(Text=str, Size=Size(x,y))
    | _,_,_ -> new Form(Text="Nim Game", Size=Size(800,800))


let buttonList = createButtons

let addButtons (window:Form) (buttons: Button list) = 
    let flp = new FlowLayoutPanel(Name="buttonPanel", AutoSize=true, Location=Point(100,100), FlowDirection = FlowDirection.LeftToRight)
    for i in buttons do
        flp.Controls.Add i
    flp 
    //List.fold (fun (window:Form) btn -> (window.Controls.Add btn); window) window buttons

let controlsMap = Map.ofList (List.zip ["gameButton"; "randomButton";"seedButton"; "startButton";"quitButton"; "restartButton"] (buttonList))

let playerRadios = createPlayerRadios

let difficultyRadios = createDifficultyRadios 

// getDiff and getPlayer can handle empty/ bad selections by setting defaults
let rec getRadioSelection (radios:RadioButton list) : string = 
    match radios with
    | [] -> ""
    | r::rs -> if r.Checked then (r.Name) else (getRadioSelection rs)

// set default difficulty to Intermediate if nothing is selected
let getDifficulty rList =
    match (rList |> getRadioSelection) with
    | "Novice" ->           Novice
    | "Intermediate" ->     Intermediate
    | "Expert" ->           Expert
    | _        ->           Intermediate

// set default player1 to player (user)
let getPlayer rList = 
    match (rList |> getRadioSelection) with
    | "player"  ->  Player 
    | "ai"      ->  PC
    | _     ->      Player 

// Initialization

let window = createWindow " " 0 0

let initialize() = 
    let buttonPanel = addButtons window buttonList

    let difficultyPanel = new FlowLayoutPanel(Name="difficultyPanel",AutoSize=true, Location = Point(400,300), FlowDirection = FlowDirection.TopDown)
    // List.iter (difficultyGroupBox.Controls.Add) difficultyRadios
    for i in difficultyRadios do
        difficultyPanel.Controls.Add i
    

    let playerPanel = new FlowLayoutPanel(Name="playerPanel",AutoSize=true, Location=Point(600,600),FlowDirection = FlowDirection.TopDown)
    // List.iter (playerGroupBox.Controls.Add) playerRadios
    for i in playerRadios do
        playerPanel.Controls.Add i
    
    let radioPanel = new FlowLayoutPanel(Name="radioPanel",AutoSize=true, Location=Point(100,200),FlowDirection = FlowDirection.LeftToRight)
    radioPanel.Controls.Add difficultyPanel
    radioPanel.Controls.Add playerPanel
   
    window.Controls.Add radioPanel
    window.Controls.Add buttonPanel
    ()

let randomGame (func: (unit-> unit)) = (controlsMap.Item "randomButton").Click.Add (fun _ -> func())

let loadGame (func:(string-> unit)) = (controlsMap.Item "seedButton").Click.Add (fun _ -> func("https://www.google.dk")) 

let startGame (func: (Player -> Difficulty -> unit)) = 
    let p = (getPlayer playerRadios)
    let diff = (getDifficulty difficultyRadios)
    (controlsMap.Item "startButton").Click.Add (fun _ -> func p diff)
    ()

// Need to implement more comprehensive choosing function here
let chooseMove (func: (int * int -> unit)) = (controlsMap.Item "gameButton").Click.Add (fun _ -> func(1,0))


let rec clearRadios (rList:RadioButton list) =
    match rList with
    | [] -> rList
    | r::rs -> (r |> (fun r -> (r.Checked <- false);r))::(clearRadios rs)   

let rec disableRadios (rList:RadioButton list) = 
    match rList with 
    | [] -> rList
    | r::rs -> (r|> (fun r -> (r.Enabled <- false); r))::(disableRadios rs)

let rec enableRadios (rList: RadioButton list) = 
    match rList with
    | [] -> rList
    | r::rs -> (r|> (fun r -> (r.Enabled <- true);r))::(enableRadios rs)

let rec disableButtons (bList: Button list) = 
    match bList with
    | [] -> bList
    | b::bs -> (b|> (fun b -> (b.Enabled <- false);b))::(disableButtons bs)

let rec enableButtons (bList: Button list) = 
    match bList with
    | [] -> bList
    | b::bs -> (b|> (fun b -> (b.Enabled <- true);b))::(enableButtons bs)

let disableAll() = 
    disableButtons buttonList |> ignore
    disableRadios difficultyRadios |> ignore
    disableRadios playerRadios |> ignore
    ()

// val cancel : (unit -> unit) -> unit
let cancel (func:(unit -> unit)) = (controlsMap.Item "quitButton").Click.Add (fun _ -> func())

// val restart : (unit -> unit) -> unit
let restart (func:(unit->unit)) = (controlsMap.Item "restartButton").Click.Add (fun _ -> func())

// Show GUI
// val show : unit -> unit
let show() = Application.Run(window)



// Interactions
// val taunt : string -> unit
let taunt str = 
    MessageBox.Show(str) |> ignore
   (* match str with
    | str ->    let popup = new Form(Text=str, Size=Size(300,300))
                Application.Run(popup) *)


// State changes
// val init : unit -> unit
let init() = 
    disableAll()
    enableButtons [(controlsMap.Item "seedButton"); (controlsMap.Item "randomButton")] |> ignore; ()

// val generatingRandom : unit -> unit
let generatingRandom() = 
    disableAll()

// val getGame : unit -> unit
let getGame() = 
    disableAll()

// val ready : Game -> unit
let ready (game:Game) = 
    disableAll()
    enableButtons [(controlsMap.Item "startButton"); (controlsMap.Item "restartButton")] |> ignore
    clearRadios  (difficultyRadios @ playerRadios) |> ignore 
    enableRadios (difficultyRadios @ playerRadios) |> ignore  
    ()

// val inProgressC : Game -> unit
let inProgressC (game:Game) = 
    disableAll()
    (controlsMap.Item "gameButton").Text <- (sprintf "%A" game)
    
// val inProgressP : Game -> unit
let inProgressP (game:Game) = 
    disableAll()
    (controlsMap.Item "gameButton").Text <- (sprintf "%A" game)
    enableButtons [(controlsMap.Item "gameButton"); (controlsMap.Item "restartButton")] |> ignore
    ()

// val moving : Game -> unit
let moving (game:Game) = 
    disableAll()

// val finish : Player -> unit
let finish (player:Player) = 
    disableAll()
    let msg = 
        match player with
        | Player ->    "Player Won!"
        | PC     ->    "PC Won!"  
    //let popup = new Form(Text=msg, Size=Size(300,300))
    MessageBox.Show(msg) |> ignore
    enableButtons [(controlsMap.Item "restartButton")] |> ignore
    //Application.Run(popup)

(*To Add: Restart Button!*)




