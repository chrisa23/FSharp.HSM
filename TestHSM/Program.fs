module Program

open FSharp.HSM
open System
    
type State = 
    | S
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
    | I
    
type HSM() = 
    let mutable foo = false
    let isFoo() = foo = true
    let notFoo() = foo = false
    let enter s = fun () -> Console.Write (sprintf "%A-ENTER;" s)
    let exit s = fun () -> Console.Write (sprintf "%A-EXIT;" s)
    let hsm = 
        [
            configure S
                |> onEntry (enter S)
                |> onExit (exit S)
                |> handleIf I isFoo (fun _ -> foo <- false; None)
                |> transitionTo S11
                
            configure S1
                |> onEntry (enter S1)
                |> onExit (exit S1)
                |> substateOf S
                |> transitionTo S11
                |> on A S1
                |> on B S11
                |> on C S21
                |> handleIf D notFoo (fun _ -> foo <- true; Some(S))
                |> on F S211
            
            configure S11
                |> onEntry (enter S11)
                |> onExit (exit S11)
                |> substateOf S1
                |> handleIf D isFoo (fun _ -> foo <- false; Some(S))
                |> on G S211
                |> on H S

            configure S2
                |> onEntry (enter S2)
                |> onExit (exit S2)
                |> substateOf S
                |> handleIf I notFoo (fun _ -> foo <- true; None)
                |> transitionTo S211
                |> on C S1
                |> on F S11

            configure S21
                |> onEntry (enter S21)
                |> onExit (exit S21)
                |> substateOf S2
                |> transitionTo S211
                |> on A S21
                |> on B S211
                |> on G S1
                
            configure S211
                |> onEntry (enter S211)
                |> onExit (exit S211)
                |> substateOf S21
                |> on D S21
                |> on H S

        ] |> create

    member this.Fire s = hsm.Fire s
    member this.Init s = hsm.Init s

[<EntryPoint>]
let main argv = 
    let hsm = HSM()
    hsm.Init S2
    let rec run() = 
        Console.WriteLine()
        Console.Write(">")
        let a = Console.ReadKey()
        Console.WriteLine()
        match a.KeyChar with
        | 'A' | 'a' -> hsm.Fire A; run()
        | 'B' | 'b' -> hsm.Fire B; run()
        | 'C' | 'c' -> hsm.Fire C; run()
        | 'D' | 'd' -> hsm.Fire D; run()
        | 'E' | 'e' -> hsm.Fire E; run()
        | 'F' | 'f' -> hsm.Fire F; run()
        | 'G' | 'g' -> hsm.Fire G; run()
        | 'H' | 'h' -> hsm.Fire H; run()
        | 'I' | 'i' -> hsm.Fire I; run()
        | _ -> ()
    run()
    0 // return an integer exit code
