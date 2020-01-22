// Prelude
open System 
open System.Net 
open System.Threading 
open System.Windows.Forms
open System.Drawing



module GUI =

    type button = 
        | NewGame of Button 
        | Random of Button 
        | Seed of Button
        | Difficulty of System.Windows.Forms.RadioButton
        | Quit of Button

    type box = 
        | Welcome of string*MessageBox
        | GameBox of int*int*Boolean*GroupBox
        | Match of int*int*Boolean*Button
    
    type window = 
        | MainWindow of int*int*Form

    // user wins = true, user loses = false
    type popup = 
        | Taunt of string*Form
        | Outcome of Boolean*Form


    let createButtons = 
        let 


    let window = new Form(Text="Nim Game", Size=Size(800,500))
    Application.Run(window)

