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
      { Current = State.OffHook
        States = 
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
              |> permit Trigger.HungUp State.OffHook ] }

    let showState() = 
      printfn "%A" phoneCall.Current
    let fire trigger = phoneCall.Fire trigger
    
    open NUnit.Framework
    open FsUnit
    
    [<Test>]
    let ``Call``() =
      fire Trigger.CallDialed
      phoneCall.IsInState(State.Connected) |> should equal false
      showState()
      fire Trigger.CallConnected
      phoneCall.IsInState(State.Connected) |> should equal true
      phoneCall.IsInState(State.OnHold) |> should equal false
      showState()
      fire Trigger.PlacedOnHold
      phoneCall.IsInState(State.Connected) |> should equal true
      phoneCall.IsInState(State.OnHold) |> should equal true
      showState()
      fire Trigger.TakenOffHold
      phoneCall.IsInState(State.Connected) |> should equal true
      phoneCall.IsInState(State.OnHold) |> should equal false
      showState()
      fire Trigger.HungUp
      phoneCall.IsInState(State.Connected) |> should equal false
      showState()
      phoneCall.Current |> should equal State.OffHook
