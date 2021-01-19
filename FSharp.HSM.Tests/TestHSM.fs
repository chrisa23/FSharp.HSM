module TestHSM

open FSharp.HSM
open System
open NUnit.Framework
open FsUnit

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

type ComplexHSM() =
    let mutable foo = false

    let hsm =
        [ configure S0
          |> onEntry (fun () -> printfn "Enter S0")
          |> onExit (fun () -> printfn "Exit S0")
          |> transitionTo S1
          |> on E S211
          configure S1
          |> onEntry (fun () -> printfn "Enter S1")
          |> onExit (fun () -> printfn "Exit S1")
          |> substateOf S0
          |> transitionTo S11
          |> on A S1
          |> on B S11
          |> on C S211
          |> on D S0
          |> on F S211
          configure S11
          |> onEntry (fun () -> printfn "Enter S11")
          |> onExit (fun () -> printfn "Exit S11")
          |> substateOf S1
          |> on G S211
          |> handleIf
              H
              (fun () -> foo)
              (fun arg ->
                  printfn "fooFal"
                  foo <- false
                  None)
          configure S2
          |> onEntry (fun () -> printfn "Enter S2")
          |> onExit (fun () -> printfn "Exit S2")
          |> substateOf S0
          |> on C S1
          |> on F S11

          configure S21
          |> onEntry (fun () -> printfn "Enter S21")
          |> onExit (fun () -> printfn "Exit S21")
          |> substateOf S2
          |> transitionTo S211
          |> on B S211
          |> handleIf
              H
              (fun () -> not foo)
              (fun arg ->
                  printfn "fooTru"
                  foo <- true
                  Some(S21))

          configure S211
          |> onEntry (fun () -> printfn "Enter S211")
          |> onExit (fun () -> printfn "Exit S211")
          |> substateOf S21
          |> on D S21
          |> on G S0 ]
        |> create

    member this.Hsm = hsm

let fireF (hsm: IStateMachine<State, Sig, unit, unit>) signal =
    printfn "fire %A" signal
    hsm.Fire(signal)

//todo:  add checks for state, entry, exits...
[<Test>]
let HsmTest () =
    let hsm = (new ComplexHSM()).Hsm
    hsm.Init S0
    let fire = fireF hsm
    fire A
    fire H
    fire E
    fire E
    //error
    try
        fire A
    with _ -> ()

    fire H
    //we should not exit S2 and S0 here... plus we should enter S21
    //foo should be set and not allow next transition...
    fire H
    fire G
    //not doing actions for now
    //fire H
    //fire H
    fire A
    fire H
