module Bug
//
//open FSharp.HSM
//
//type State = 
// | Open
// | Assigned 
// | Deferred
// | Resolved
// | Closed
//
//type Trigger= 
//
// | Defer
// | Resolve 
// | Close
// //this won't work because it is a string -> Trigger...  doesn't support equality...???
// | Assign //of string
//
////separate the data from the trigger type
//type Data =
//    | Assign of string
//
//let mutable assignee = ""
//
//let onAssigned(trigger:obj) = 
//    match unbox trigger  with
//    | Assign name-> 
//        assignee <- name
//        Some(State.Assigned)
//    | _ -> 
//        assignee <- ""
//        None;
//
//    
//
////
//let machine = 
//    [ configure State.Open
//        |> on Trigger.Assign State.Assigned //??? is there a way to do this?
//        |> handle Trigger.Assign onAssigned
//      configure State.Assigned
//        |> substateOf State.Open
////        |> permitF Trigger.Assign (fun x -> 
////            onAssigned(y)
////            State.Assigned)
//        |> on Trigger.Close State.Closed
//        |> on Trigger.Defer State.Deferred
//       // |> onExit (fun _ -> OnDeassigned())
//       // |> permitF Trigger.Assign (fun x -> State.Assigned)
//      configure State.Deferred
//        |> onEntry (fun _ -> ())
//        //|> on Trigger.Assign State.Assigned 
//        ]
//
//    |> create
