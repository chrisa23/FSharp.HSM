module TestHelpers

open FSharp.HSM
open FsUnit
   
let fire (phoneCall:StateMachine<_,_>) trigger = 
    printfn "fire %A" trigger
    phoneCall.Fire trigger

let fireW (trade:StateMachine<_,_>) trigger data = 
    printfn "fire %A" trigger
    trade.Fire(trigger, data)

let attachShow (phoneCall:StateMachine<_,_>) = phoneCall.StateChanged.Add (fun state -> printfn "%A" state)
let isInState (phoneCall:StateMachine<_,_>) state = phoneCall.IsIn state |> should equal true
let isNotInState (phoneCall:StateMachine<_,_>) state = phoneCall.IsIn state |> should equal false


