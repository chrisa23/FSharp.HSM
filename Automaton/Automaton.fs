module Automaton 

open System.Collections.Generic

type Transition<'state,'event> = 
  { Trigger: 'event 
    State: 'state
    Guard: unit -> bool }
    
type State<'state,'event> = 
  { State: 'state
    SuperState: 'state option
    Entry: 'event -> unit
    Exit: unit -> unit
    Reentry: bool
    Transitions: Transition<'state,'event> list}

exception NoTransition

let isSub state = 
  match state.SuperState with
  | None -> false
  | Some _ -> true

let isSuper super state = 
  match super.SuperState with
  | None -> false
  | Some x -> x = state

type StateMachine<'state,'event when 'state : equality and 'event :equality>(states:State<'state,'event> list ) = 
    let mutable current = states.Head.State
    let find state = states |> List.find (fun x -> x.State = state)
    let stateEvent = new Event<'state>()
    member this.State with get() = current
    member this.StateChanged = stateEvent.Publish
    member this.IsIn (state:'state) = current = state || isSuper (find current) state 
    member this.Fire(trigger)= 
        let cur = find current
        let mutable trans = Unchecked.defaultof<Transition<'state,'event>>
        match cur.Transitions |> List.tryFind (fun x -> x.Trigger = trigger) with
        | None -> 
            match cur.SuperState with 
            | None -> raise NoTransition//error
            | Some x -> trans <- (find x).Transitions  |> List.find (fun x -> x.Trigger = trigger)//do we have a superstate?
        | Some x ->  trans <- x

        if trans.Guard() then 
            let newCur = trans.State
            let newState = states |> List.find (fun x -> x.State = newCur)
        
            // transition from super to substate
            let isSelf = cur.State = newState.State
            let moveToSub = isSuper newState cur.State
            let curIsSub = isSub cur 
            let newIsSuper = isSuper cur newState.State

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
                newState.Entry(trigger)

            current <- newCur
            stateEvent.Trigger current
    //member this.PermittedEvents ?

let configure state = 
  { State = state; SuperState = None; Entry = (fun _ -> ()); Exit = (fun _ -> ()); Reentry = false; Transitions = [] }

let substateOf superState state = { state with SuperState = Some(superState) }
  
let onEntry f state = {state with Entry = (fun _ -> f()) }
let onEntryFrom prevState f state = {state with Entry = (fun x -> f(x)) }

let onExit f state = {state with Exit = f }

let permit trigger endState state =
  { state with Transitions = { Trigger = trigger; State = endState; Guard = (fun _ -> true) }::state.Transitions }

let permitIf trigger endState guard state = { state with Transitions = { Trigger = trigger;State = endState; Guard = guard }::state.Transitions }

let permitReentry trigger state = 
  { state with Reentry = true; Transitions = { Trigger = trigger; State = state.State; Guard = (fun _ -> true) }::state.Transitions }

let permitReentryIf trigger guard state = 
  { state with Reentry = true; Transitions = { Trigger = trigger; State = state.State; Guard = guard }::state.Transitions }
//todo:
//permitReentry?
//ignore

