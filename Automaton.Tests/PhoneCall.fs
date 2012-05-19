namespace AutomatonTests  

module PhoneCall =
    open Automaton
    open System

    type State = 
     | OffHook
     | Ringing
     | Connected 
     | OnHold

    type Trigger =
     | CallDialed 
     | HungUp 
     | CallConnected 
     | PlacedOnHold
     | TakenOffHold

    let mutable timerOn = false
    let startTimer() = 
      printfn "%A connected" DateTime.Now
      timerOn <- true
    let stopTimer() = 
      printfn "%A ended" DateTime.Now
      timerOn <- false

    let phoneCall = 
      new StateMachine<State,Trigger>(
          [ configure State.OffHook
              |> permit Trigger.CallDialed State.Ringing
            configure State.Ringing
              |> permit Trigger.CallConnected State.Connected
            configure State.Connected
              |> permit Trigger.PlacedOnHold State.OnHold
              |> permit Trigger.HungUp State.OffHook
              |> onEntry (fun _ -> startTimer())
              |> onExit (fun _ -> stopTimer())
            configure State.OnHold
              |> substateOf State.Connected
              |> permit Trigger.TakenOffHold State.Connected ] )

    let showState state = printfn "%A" state
    phoneCall.StateChanged.Add showState
    
    let fire trigger = phoneCall.Fire trigger
    
    open NUnit.Framework
    open FsUnit
    
    let check trueStates falseStates  (timer: bool) = 
        timerOn |> should equal timer
        trueStates |> List.iter (fun x ->
           phoneCall.IsIn x |> should equal true)
        falseStates |> List.iter (fun x ->
           phoneCall.IsIn x |> should equal false)

    [<Test>]
    let ``Call``() =

      fire Trigger.CallDialed
      
      check [ State.Ringing ] [State.Connected; State.OnHold; State.OffHook ] false
      
      fire Trigger.CallConnected
      
      check [ State.Connected ] [State.Ringing; State.OnHold; State.OffHook ] true
      
      fire Trigger.PlacedOnHold

      check [ State.Connected; State.OnHold ] [State.Ringing; State.OffHook ] true
      
      fire Trigger.HungUp//i should be able to hang up here based on Connected state

      check [ State.OffHook; ] [State.Ringing; State.Connected; State.OnHold ] false
      
      fire Trigger.CallDialed

      check [ State.Ringing ] [State.Connected; State.OnHold; State.OffHook ] false

      fire Trigger.CallConnected

      check [ State.Connected ] [State.Ringing; State.OnHold; State.OffHook ] true

      fire Trigger.PlacedOnHold
      
      check [ State.Connected; State.OnHold ] [State.Ringing; State.OffHook ] true

      fire Trigger.TakenOffHold

      check [ State.Connected ] [State.Ringing; State.OnHold; State.OffHook ] true

      fire Trigger.HungUp

      check [ State.OffHook; ] [State.Ringing; State.Connected; State.OnHold ] false
