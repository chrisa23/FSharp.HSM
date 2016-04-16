module TestHelpers

open FSharp.HSM
open FsUnit
   
let fire (phoneCall:IStateMachine<_,_>) trigger = 
    printfn "fire %A" trigger
    phoneCall.Fire trigger

let fireW (trade:IStateMachine<_,_>) trigger data = 
    printfn "fire %A" trigger
    trade.Fire(trigger, data)

let attachShow (hsm:IStateMachine<_,_>) = hsm.StateChanged.Add (fun state -> printfn "%A" state)
let isInState (hsm:IStateMachine<_,_>) state = hsm.IsIn state |> should equal true
let isNotInState (hsm:IStateMachine<_,_>) state = hsm.IsIn state |> should equal false


