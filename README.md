FSharp.HSM
==========

Hierarchical State Machine library in F#

Inspired by http://code.google.com/p/stateless/, http://accu.org/index.php/journals/252

Examples:


Phone call:

    new StateMachine < State , Trigger > (
        [ configure OffHook
            |> on CallDialed Ringing
          configure Ringing
            |> on CallConnected Connected
          configure Connected
            |> onEntry startTimer
            |> onExit stopTimer
            |> transitionTo InCall
            |> on HungUp OffHook
          configure InCall
            |> substateOf Connected
            |> on PlacedOnHold OnHold
          configure OnHold
            |> substateOf Connected
            |> on TakenOffHold InCall ] )

Complex HSM:

    new StateMachine < State , Signal > (
        [ configure S0
            |> transitionTo S1
            |> on E S211
          configure S1
            |> substateOf S0
            |> transitionTo S11
            |> on A S1 
            |> on B S11
            |> on C S211 
            |> on D S0 
            |> on F S211 
          configure S11
            |> substateOf S1
            |> on G S211 
            |> handleIf H (fun () -> foo) (fun event arg -> printfn "fooFal"; foo <- false; S11)
          configure S2
            |> substateOf S0
            |> on C S1 
            |> on F S11 
          configure S21
            |> substateOf S2
            |> transitionTo S211
            |> on B S211 
            |> handleIf H (fun () -> not foo) (fun event arg -> printfn "fooTru"; foo <- true; S21 )
          configure S211
            |> substateOf S21
            |> on D S21
            |> on G S0 ] )
