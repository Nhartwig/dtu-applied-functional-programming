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


let createDifficultyRadios = 
    let rb1 = new RadioButton(Name="Novice",Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Novice")
    let rb2 = new RadioButton(Name="Intermediate",Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Intermediate")
    let rb3 = new RadioButton(Name="Expert",Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Expert")
    [rb1; rb2; rb3]


let createPlayerRadios = 
    let playerFirst = new RadioButton(Name="player" ,Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Go First")
    let aiFirst = new RadioButton(Name="ai", Location=Point(100,1),MinimumSize=Size(10,10),MaximumSize=Size(10,10),Text="Let AI Go First")
    [playerFirst; aiFirst]


let createButtons = 
    let gameButton = new Button(Name="gameButton",Location=Point(100,1),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="New Game!")
    let randomButton = new Button(Name="randomButton",Location=Point(200,1),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Random Game")
    let seedButton = new Button(Name="seedButton",Location=Point(300,1),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Seed from Web")
    let startButton = new Button(Name="startButton",Location=Point(400,1),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Start Game!")
    let quitButton = new Button(Name="quitButton", Location=Point(500,1),MinimumSize=Size(100,50),MaximumSize=Size(100,50),Text="Quit Game ;(")
    [gameButton; randomButton; seedButton; startButton; quitButton]

let createWindow str x y = 
    match str, x, y with
    | str, x, y when (((x > 0) && (y > 0)) && ((String.length (str)) > 0))-> new Form(Text=str, Size=Size(x,y))
    | _,_,_ -> new Form(Text="Nim Game", Size=Size(800,800))


let buttonList = createButtons

let addButtons (window:Form) (buttons: Button list) = List.fold (fun (window:Form) btn -> (window.Controls.Add btn); window) window buttons

let controlsMap = Map.ofList (List.zip ["gameButton"; "randomButton";"seedButton"; "startButton";"quitButton"] (buttonList))

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
    let window = addButtons window buttonList

    let difficultyGroupBox = new GroupBox(Name="difficultyGroupBox",AutoSize=true)
    List.iter (difficultyGroupBox.Controls.Add) difficultyRadios

    let playerGroupBox = new GroupBox(Name="playerGroupBox",AutoSize=true)
    List.iter (playerGroupBox.Controls.Add) playerRadios

    window.Controls.Add difficultyGroupBox
    window.Controls.Add playerGroupBox
    ()

let randomGame (func: (unit-> unit)) = (controlsMap.Item "randomButton").Click.Add (fun _ -> func())

let loadGame (func:(string-> unit)) = (controlsMap.Item "seedButton").Click.Add (fun _ -> func("www.google.dk")) 

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


let cancel (func:(unit -> unit)) = ()

let restart (func:(unit->unit)) = ()




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






