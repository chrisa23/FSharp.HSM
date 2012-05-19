module Automaton 

open System.Collections.Generic

//how do I make these internal?
type Transition<'state,'event> = 
  { Event: 'event 
    State: 'state
    Guard: unit -> bool }
    
type StateConfig<'state,'event> = 
  { State: 'state
    SuperState: 'state option
    Entry: 'event -> unit
    Exit: unit -> unit
    Reentry: bool
    Transitions: Transition<'state,'event> list}

exception NoTransition

type StateMachine<'state,'event when 'state : equality and 'event :equality>(states:StateConfig<'state,'event> list ) = 
    let isSub state = 
      match state.SuperState with
      | None -> false
      | Some _ -> true
    let isSuper state subState = 
      match subState.SuperState with
      | None -> false
      | Some x -> x = state
    let mutable current = states.Head.State
    let find state = states |> List.find (fun x -> x.State = state)
    let stateEvent = new Event<'state>()
    member this.State with get() = current
    member this.StateChanged = stateEvent.Publish
    member this.IsIn (state:'state) = current = state || isSuper state (find current) 
    member this.Fire event = 
        let cur = find current
        let mutable trans = Unchecked.defaultof<Transition<'state,'event>>
        match cur.Transitions |> List.tryFind (fun x -> x.Event = event) with
        | None -> 
            match cur.SuperState with //do we have a superstate?
            | None -> raise NoTransition//error
            | Some x -> trans <- (find x).Transitions |> List.find (fun x -> x.Event = event)//error if not found
        | Some x ->  trans <- x
        if trans.Guard() then 
            let newCur = trans.State
            let newState = states |> List.find (fun x -> x.State = newCur)
            let isSelf = cur.State = newState.State
            let moveToSub = isSuper cur.State newState
            let curIsSub = isSub cur 
            let newIsSuper = isSuper newState.State cur 

            if (not(moveToSub) && not(isSelf)) 
                || (isSelf && cur.Reentry)  then  
                cur.Exit()
            //but exit of super if sub -> not super
            //if we are in a substate and not transition to super
            //call the super's exit
            if not(newIsSuper) && curIsSub then
                match cur.SuperState with
                | None -> ()
                | Some super -> (find super).Exit()                    

            //if not transition from sub to super
            if (not(newIsSuper) && not(isSelf)) 
               || (isSelf && cur.Reentry)  then 
                newState.Entry(event)

            current <- newCur
            stateEvent.Trigger current
    //member this.PermittedEvents ?  //collect transitions and any superstate transitions...

let configure state = 
  { State = state; SuperState = None; Entry = (fun _ -> ()); Exit = (fun _ -> ()); Reentry = false; Transitions = [] }

let substateOf superState state = { state with SuperState = Some(superState) }
  
let onEntry f state = {state with Entry = (fun _ -> f()) }
let onEntryFrom prevState f state = {state with Entry = (fun x -> f(x)) }

let onExit f state = {state with Exit = f }

let permit event endState state =
  { state with Transitions = { Event = event; State = endState; Guard = (fun _ -> true) }::state.Transitions }

let permitIf event endState guard state = { state with Transitions = { Event = event;State = endState; Guard = guard }::state.Transitions }

let permitReentry event state = 
  { state with Reentry = true; Transitions = { Event = event; State = state.State; Guard = (fun _ -> true) }::state.Transitions }

let permitReentryIf event guard state = 
  { state with Reentry = true; Transitions = { Event = event; State = state.State; Guard = guard }::state.Transitions }

//todo:
//ignore
//unhandled handler
