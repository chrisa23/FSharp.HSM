module Bug

open Automaton

type State = 
 | Open
 | Assigned 
 | Deferred
 | Resolved
 | Closed

type Trigger= 

 | Defer
 | Resolve 
 | Close
 //i think this won't work since we lose the pattern matching of 
 | Assign of string

//let (|Assign|_|) trigger = 
let mutable assignee = ""

let onAssigned(trigger) = 
    match trigger with
    | Assign name -> assignee <- name
    | _ -> assignee <- ""

//
//let machine = 
// {Current = ref State.Open
//  States = 
//    [ configure State.Open
//        |> permitF Trigger.Assign (fun x -> 
//            onAssigned(x)
//            State.Assigned)
//      configure State.Assigned
//        |> substateOf State.Open
//        |> permitF Trigger.Assign (fun x -> 
//            onAssigned(y)
//            State.Assigned)
//        |> permit Trigger.Close State.Closed
//        |> permit Trigger.Defer State.Deferred
//        |> onExit (fun _ -> OnDeassigned())
//        |> permitF Trigger.Assign (fun x -> State.Assigned)
//      configure State.Deferred
//        |> onEntry (fun _ -> ())
//        |> permit Trigger.Assign State.Assigned ] }
