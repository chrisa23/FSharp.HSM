module TestHelpers

open FSharp.HSM
open FsUnit
   
let fire (phoneCall:IStateMachine<_,_>) trigger = 
    printfn "fire %A" trigger
    phoneCall.Fire trigger

let fireW (trade:IStateMachine<_,_>) trigger data = 
    printfn "fire %A" trigger
    trade.Fire(trigger, data)

let attachShow (phoneCall:IStateMachine<_,_>) = phoneCall.StateChanged.Add (fun state -> printfn "%A" state)
let isInState (phoneCall:IStateMachine<_,_>) state = phoneCall.IsIn state |> should equal true
let isNotInState (phoneCall:IStateMachine<_,_>) state = phoneCall.IsIn state |> should equal false


