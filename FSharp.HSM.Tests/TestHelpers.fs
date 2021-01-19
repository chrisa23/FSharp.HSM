module TestHelpers

open FSharp.HSM
open FsUnit

let fire (hsm: IStateMachine<_, _, _, _>) trigger =
    printfn "fire %A" trigger
    hsm.Fire trigger

let fireW (hsm: IStateMachine<_, _, _, _>) trigger data =
    printfn "fire %A" trigger
    hsm.Fire(trigger, data)

let attachShow (hsm: IStateMachine<_, _, _, _>) =
    hsm.StateChanged.Add(fun state -> printfn "%A" state)

let attachHistory (hsm: IStateMachine<'state, _, _, _>) (store: ResizeArray<'state>) =
    hsm.StateChanged.Add(fun state -> store.Add state)

let attachOutputHistory (hsm: IStateMachine<_, _, 'output, _>) (store: ResizeArray<'output>) =
    hsm.EventRaised.Add(fun output -> store.Add output)

let isInState (hsm: IStateMachine<_, _, _, _>) state = hsm.IsIn state |> should equal true
let isNotInState (hsm: IStateMachine<_, _, _, _>) state = hsm.IsIn state |> should equal false



