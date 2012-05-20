FSharp.HSM
==========

Hierarchical State Machine library in F#

Inspired by stateless (http://code.google.com/p/stateless/), http://accu.org/index.php/journals/252

Examples:

<pre>
Phone call:
    new StateMachine<State,Trigger>(
        [ configure OffHook
            |> on CallDialed Ringing
          configure Ringing
            |> on CallConnected Connected
          configure Connected
            |> onEntry (fun _ -> startTimer())
            |> onExit (fun _ -> stopTimer())
            |> transitionTo InCall
            |> on HungUp OffHook
          configure InCall
            |> substateOf Connected
            |> on PlacedOnHold OnHold
          configure OnHold
            |> substateOf Connected
            |> on TakenOffHold InCall ] )

Complex HSM:
    new StateMachine<State,Sig>(
        [ configure S0
            |> onEntry (fun _ -> printfn "Enter S0")
            |> onExit (fun _ -> printfn "Exit S0")
            |> transitionTo S1
            |> on E S211
          configure S1
            |> onEntry (fun _ -> printfn "Enter S1")
            |> onExit (fun _ -> printfn "Exit S1")
            |> substateOf S0
            |> transitionTo S11
            |> on A S1 
            |> on B S11
            |> on C S211 
            |> on D S0 
            |> on F S211 
          configure S11
            |> onEntry (fun _ -> printfn "Enter S11")
            |> onExit (fun _ -> printfn "Exit S11")
            |> substateOf S1
            |> on G S211 
			|> actionIf h (fun _ -> foo) (fun _ -> foo <- false) //still todo
          configure S2
            |> onEntry (fun _ -> printfn "Enter S2")
            |> onExit (fun _ -> printfn "Exit S2")
            |> substateOf S0
            |> on C S1 
            |> on F S11 
          configure S21
            |> onEntry (fun _ -> printfn "Enter S21")
            |> onExit (fun _ -> printfn "Exit S21")
            |> substateOf S2
            |> transitionTo S211
            |> on B S211 
            |> handleIf H (fun _ -> not foo) (fun x y -> foo <- true; S21 )
          configure S211
            |> onEntry (fun _ -> printfn "Enter S211")
            |> onExit (fun _ -> printfn "Exit S211")
            |> substateOf S21
            |> on D S21
            |> on G S0 ] ) </pre>
