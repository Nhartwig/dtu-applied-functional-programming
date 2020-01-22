module GameLogic

type Game = int list
val move       : int -> int -> Game -> Game
val randomGame : int -> Game
val getGame    : string -> Game