module Automaton 

open System.Collections.Generic

type Transition<'state,'event> = 
  { Trigger: 'event 
    Func: 'event -> 'state }
    
type State<'state,'event> = 
  { State: 'state
    SuperState: 'state option
    Entry: unit -> unit
    Exit: unit -> unit
    Transitions: Transition<'state,'event> list}

type StateMachine<'state,'event when 'state : equality and 'event :equality>(states:State<'state,'event> list ) = 
    let mutable current = states.Head.State
    //how do I make this efficient..
    let find state = states |> List.find (fun x -> x.State = state)
    let stateEvent = new Event<'state>()
    member this.State with get() = current
    member this.StateChanged = stateEvent.Publish
    member this.IsIn(state:'state) = 
        if current = state then 
          true
        else
          match find(current).SuperState with
          | None -> false
          | Some x -> x = state
    member this.Fire(trigger)= 
        let cur = find(current)
        cur.Exit()
        let trans = cur.Transitions |> List.find (fun x -> x.Trigger = trigger)
        let newCur = trans.Func(trigger)
        let newState = states |> List.find (fun x -> x.State = newCur)
        newState.Entry()
        current <- newCur
        stateEvent.Trigger current

let configure state = 
  { State = state; SuperState = None; Entry = (fun _ -> ()); Exit = (fun _ -> ()); Transitions = [] }

let substateOf superState state = { state with SuperState = Some(superState) }
  
let onEntry f state = {state with Entry = f }
let onExit f state = {state with Exit = f }

let permit trigger endState state =
  { state with Transitions = { Trigger = trigger; Func = (fun _ -> endState) }::state.Transitions }

let permitF trigger f state = { state with Transitions = { Trigger = trigger; Func = f }::state.Transitions }



