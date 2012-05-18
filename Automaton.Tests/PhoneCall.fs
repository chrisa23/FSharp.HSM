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

    let startTimer() = printfn "%A connected" DateTime.Now
    let stopTimer() = printfn "%A ended" DateTime.Now

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
              |> permit Trigger.TakenOffHold State.Connected
              |> permit Trigger.HungUp State.OffHook ] )
    

    let showState state = printfn "%A" state
    phoneCall.StateChanged.Add showState
    
    let fire trigger = phoneCall.Fire trigger
    
    open NUnit.Framework
    open FsUnit
    
    [<Test>]
    let ``Call``() =

      fire Trigger.CallDialed
      phoneCall.IsIn(State.Connected) |> should equal false
      
      fire Trigger.CallConnected
      phoneCall.IsIn(State.Connected) |> should equal true
      phoneCall.IsIn(State.OnHold) |> should equal false
      
      fire Trigger.PlacedOnHold
      phoneCall.IsIn(State.Connected) |> should equal true
      phoneCall.IsIn(State.OnHold) |> should equal true
      
      fire Trigger.TakenOffHold
      phoneCall.IsIn(State.Connected) |> should equal true
      phoneCall.IsIn(State.OnHold) |> should equal false
      
      fire Trigger.HungUp
      phoneCall.IsIn(State.Connected) |> should equal false
      phoneCall.State |> should equal State.OffHook
