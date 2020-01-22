module GUI
type Player = Player | PC
type Game = int list
type Difficulty = Novice | Intermediate | Expert
// Initialization
val initialize : unit -> unit
val randomGame : (unit -> unit) -> unit
val loadGame : (string -> unit) -> unit
val startGame : (Player -> Difficulty -> unit) -> unit
val chooseMove : (int * int -> unit) -> unit
val cancel : (unit -> unit) -> unit
val restart : (unit -> unit) -> unit

// Show GUI
val show : unit -> unit

// Interactions
val taunt : string -> unit

// State changes
val init : unit -> unit
val generatingRandom : unit -> unit
val getGame : unit -> unit
val ready : Game -> unit
val inProgressC : Game -> unit
val inProgressP : Game -> unit
val moving : Game -> unit
val finish : Player -> unit
