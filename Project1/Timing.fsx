// Created Luke Merrett
// Copied from https://lukemerrett.com/timing-a-function-in-fsharp/
module Timing

open System.Diagnostics

type TimedOperation<'T> = {millisecondsTaken:int64; returnedValue:'T}

let timeOperation<'T> (func: unit -> 'T): TimedOperation<'T> =
    let timer = new Stopwatch()
    timer.Start()
    let returnValue = func()
    timer.Stop()
    {millisecondsTaken=timer.ElapsedMilliseconds; returnedValue=returnValue}
