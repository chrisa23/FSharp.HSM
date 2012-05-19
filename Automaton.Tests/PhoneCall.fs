namespace AutomatonTests  

open Automaton
open System
open NUnit.Framework
open FsUnit

module PhoneCallTest =
    
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

    let newPhoneCall() = 
      new StateMachine<State,Trigger>(
          [ configure State.OffHook
              |> permit Trigger.CallDialed State.Ringing
            configure State.Ringing
              |> permit Trigger.CallConnected State.Connected
            configure State.Connected
              |> onEntry (fun _ -> startTimer())
              |> onExit (fun _ -> stopTimer())
              |> permit Trigger.PlacedOnHold State.OnHold
              |> permit Trigger.HungUp State.OffHook
            configure State.OnHold
              |> substateOf State.Connected
              |> permit Trigger.TakenOffHold State.Connected ] )

    let showState state = printfn "%A" state
   
    let fire (phoneCall:StateMachine<State,Trigger>) trigger = phoneCall.Fire trigger

    let attachShow (phoneCall:StateMachine<State,Trigger>) = phoneCall.StateChanged.Add showState

    let isInState (phoneCall:StateMachine<State,Trigger>) state = phoneCall.IsIn state |> should equal true
    let isNotInState (phoneCall:StateMachine<State,Trigger>) state = phoneCall.IsIn state |> should equal false

    let check phoneCall trueStates falseStates (timer: bool) = 
        timerOn |> should equal timer
        trueStates |> List.iter (isInState phoneCall)
        falseStates |> List.iter (isNotInState phoneCall)

    [<Test>]
    let ``Dial -> Connect -> HangUp``() =
      let call = newPhoneCall()
      attachShow call

      fire call Trigger.CallDialed
      check call [ State.Ringing ] [State.Connected; State.OnHold; State.OffHook ] false
      
      fire call Trigger.CallConnected
      check call [ State.Connected ] [State.Ringing; State.OnHold; State.OffHook ] true

      fire call Trigger.HungUp
      check call [ State.OffHook; ] [State.Ringing; State.Connected; State.OnHold ] false


    [<Test>]
    let ``Dial -> Connect -> Hold -> HangUp``() =
      let call = newPhoneCall()
      attachShow call

      fire call Trigger.CallDialed
      check call [ State.Ringing ] [State.Connected; State.OnHold; State.OffHook ] false
      
      fire call Trigger.CallConnected
      check call [ State.Connected ] [State.Ringing; State.OnHold; State.OffHook ] true
      
      fire call Trigger.PlacedOnHold
      check call [ State.Connected; State.OnHold ] [State.Ringing; State.OffHook ] true
      
      fire call Trigger.HungUp//i should be able to hang up here based on Connected state
      check call [ State.OffHook; ] [State.Ringing; State.Connected; State.OnHold ] false

    [<Test>]
    let ``Dial -> Connect -> Hold -> UnHold -> HangUp``() =      
      let call = newPhoneCall()
      attachShow call

      fire call Trigger.CallDialed
      check call [ State.Ringing ] [State.Connected; State.OnHold; State.OffHook ] false

      fire call Trigger.CallConnected
      check call [ State.Connected ] [State.Ringing; State.OnHold; State.OffHook ] true

      fire call Trigger.PlacedOnHold
      check call [ State.Connected; State.OnHold ] [State.Ringing; State.OffHook ] true

      fire call Trigger.TakenOffHold
      check call [ State.Connected ] [State.Ringing; State.OnHold; State.OffHook ] true

      fire call Trigger.HungUp
      check call [ State.OffHook; ] [State.Ringing; State.Connected; State.OnHold ] false

    [<Test; ExpectedException(typeof<NoTransition>)>]
    let ``Hold -> error``() =      
      let call = newPhoneCall()
      attachShow call
      fire call Trigger.PlacedOnHold
      