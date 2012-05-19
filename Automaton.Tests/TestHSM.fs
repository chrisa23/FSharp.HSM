namespace AutomatonTests

open Automaton
open System
open NUnit.Framework
open FsUnit

module TestHSM =
    
    type State = 
     | S0
     | S1
     | S11
     | S2
     | S21
     | S211

    type Sig =
     | A 
     | B 
     | C 
     | D
     | E
     | F
     | G
     | H

    let mutable foo = false

    let newComplexHSM() = 
      new StateMachine<State,Sig>(
          [ configure S0
              |> onEntry (fun _ -> printfn "Enter S0")
              |> onExit (fun _ -> printfn "Exit S0")
              |> transitionTo S1
              |> permit E S211
            configure S1
              |> onEntry (fun _ -> printfn "Enter S1")
              |> onExit (fun _ -> printfn "Exit S1")
              |> substateOf S0
              |> transitionTo S11
              |> permit A S1 
              |> permit B S11
              |> permit C S211 
              |> permit D S0 
              |> permit F S211 
            configure S11
              |> onEntry (fun _ -> printfn "Enter S11")
              |> onExit (fun _ -> printfn "Exit S11")
              |> substateOf S1
              |> permit G S211 
              //|> onEntry (fun _ -> foo <- false)
              //onIf H guard function
            configure S2
              |> onEntry (fun _ -> printfn "Enter S2")
              |> onExit (fun _ -> printfn "Exit S2")
              |> substateOf S0
              |> permit C S1 
              |> permit F S11 
            configure S21
              |> onEntry (fun _ -> printfn "Enter S21")
              |> onExit (fun _ -> printfn "Exit S21")
              |> substateOf S2
              |> permit B S211 
              |> permitIf H S21 (fun _ -> not foo)
            configure S211
              |> onEntry (fun _ -> printfn "Enter S211")
              |> onExit (fun _ -> printfn "Exit S211")
              |> substateOf S21
              |> permit D S21
              |> permit G S0 ] ) 

    let fire (hsm:StateMachine<State,Sig>) signal = 
        printfn "fire %A" signal
        hsm.Fire signal

    [<Test>]
    let HsmTest() = 
      let hsm = newComplexHSM()
      hsm.Init S0
      fire hsm A
      fire hsm E
      fire hsm E
      //error
      try
        fire hsm A
      with 
      | NoTransition -> ()
      fire hsm H
      fire hsm H

      