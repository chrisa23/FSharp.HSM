module Bug

open FSharp.HSM

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
 //this won't work because it is a string -> Trigger...
 | Assign of string

let mutable assignee = ""

let onAssigned(trigger) = 
    match trigger with
    | Assign name -> assignee <- name
    | _ -> assignee <- ""


//let machine = 
//  new StateMachine<State,Trigger>(
//    [ configure State.Open
//        |> permit Trigger.Assign(_) State.Assigned
//      configure State.Assigned
//        |> substateOf State.Open
////        |> permitF Trigger.Assign (fun x -> 
////            onAssigned(y)
////            State.Assigned)
//        |> permit Trigger.Close State.Closed
//        |> permit Trigger.Defer State.Deferred
//        |> onExit (fun _ -> OnDeassigned())
//        |> permitF Trigger.Assign (fun x -> State.Assigned)
//      configure State.Deferred
//        |> onEntry (fun _ -> ())
//        |> permit Trigger.Assign State.Assigned ] )
