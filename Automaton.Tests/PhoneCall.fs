namespace AutomatonTests  

open Automaton
open System
open NUnit.Framework
open FsUnit

module PhoneCallTest =
    
    type State = 
     | OffHook
     | Ringing
     | Connected //composite
     | InCall
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
          [ configure OffHook
              |> permit CallDialed Ringing
            configure Ringing
              |> permit CallConnected Connected
            configure Connected
              |> onEntry (fun _ -> startTimer())
              |> onExit (fun _ -> stopTimer())
              |> transitionTo InCall
              |> permit HungUp OffHook
            configure InCall
              |> substateOf Connected
              |> permit PlacedOnHold OnHold
            configure OnHold
              |> substateOf Connected
              |> permit TakenOffHold InCall ] )

    let showState state = printfn "%A" state
   
    let fire (phoneCall:StateMachine<State,Trigger>) trigger = 
      printfn "fire %A" trigger
      phoneCall.Fire trigger

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
      call.Init OffHook
      fire call CallDialed
      check call [ Ringing ] [Connected; OnHold; OffHook ] false
      
      fire call CallConnected
      check call [ Connected ] [Ringing; OnHold; OffHook ] true

      fire call HungUp
      check call [ OffHook; ] [Ringing; Connected; OnHold ] false


    [<Test>]
    let ``Dial -> Connect -> Hold -> HangUp``() =
      let call = newPhoneCall()
      attachShow call
      call.Init OffHook
      fire call CallDialed
      check call [ Ringing ] [Connected; OnHold; OffHook; InCall ] false
      
      fire call CallConnected
      check call [ Connected; InCall ] [Ringing; OnHold; OffHook ] true
      
      fire call PlacedOnHold
      check call [ Connected; OnHold ] [Ringing; OffHook; InCall ] true
      
      fire call HungUp//i should be able to hang up here based on Connected state
      check call [ OffHook; ] [Ringing; Connected; OnHold; InCall ] false

    [<Test>]
    let ``Dial -> Connect -> Hold -> UnHold -> HangUp``() =      
      let call = newPhoneCall()
      attachShow call
      call.Init OffHook
      fire call CallDialed
      check call [ Ringing ] [Connected; OnHold; OffHook; InCall ] false

      fire call CallConnected
      check call [ Connected; InCall ] [Ringing; OnHold; OffHook ] true

      fire call PlacedOnHold
      check call [ Connected; OnHold ] [Ringing; OffHook; InCall ] true

      fire call TakenOffHold
      check call [ Connected; InCall] [Ringing; OnHold; OffHook ] true

      fire call HungUp
      check call [ OffHook; ] [Ringing; Connected; OnHold; InCall ] false

    [<Test; ExpectedException(typeof<NoTransition>)>]
    let ``Hold -> error``() =      
      let call = newPhoneCall()
      attachShow call
      call.Init OffHook
      fire call PlacedOnHold
      