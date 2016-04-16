FSharp.HSM
==========

Hierarchical State Machine library in F#

Inspired by http://code.google.com/p/stateless/, http://accu.org/index.php/journals/252

Features:
- Hierarchical states
- Entry/Exit actions
- Guards
- Auto transitions
- Event handlers with optional transitions


Examples:


Phone call:

    [ configure OffHook
        |> on CallDialed Ringing
      configure Ringing
        |> on CallConnected Connected
        |> on HungUp OffHook
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
        |> onEntry startHoldMusic
        |> onExit endHoldMusic
        |> on TakenOffHold InCall ] 
      |> HSM.create

Complex HSM:

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
        |> handleIf H (fun () -> foo) (fun event arg -> foo <- false; None)
      configure S2
        |> substateOf S0
        |> on C S1 
        |> on F S11 
      configure S21
        |> substateOf S2
        |> transitionTo S211
        |> on B S211 
        |> handleIf H (fun () -> not foo) (fun event arg -> foo <- true; Some(S21) )
      configure S211
        |> substateOf S21
        |> on D S21
        |> on G S0 ]
	|> HSM.create
