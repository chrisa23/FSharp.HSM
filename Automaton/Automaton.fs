module Automaton 

open System.Collections.Generic

type Transition<'state,'trigger> = 
  { Trigger: 'trigger 
    Func: ('trigger -> 'state) }
    
type State<'state,'trigger> = 
  { State: 'state
    SuperState: 'state option
    Entry: unit -> unit
    Exit: unit -> unit
    Transitions: Transition<'state,'trigger> list}

type StateMachine<'state,'trigger when 'state : equality and 'trigger :equality> = 
  { mutable Current: 'state 
    States: State<'state,'trigger> list }
    member this.Find state =
        this.States |> List.find (fun x -> x.State = state)
    member this.State with get() = this.Current
    member this.IsInState(state:'state) = 
        if this.Current = state then 
          true
        else
          match this.Find(this.Current).SuperState with
          | None -> false
          | Some x -> x = state
    member this.Fire(trigger)= 
        let cur = this.Find(this.Current)
        cur.Exit()
        let trans = cur.Transitions |> List.find (fun x -> x.Trigger = trigger)
        let newCur = trans.Func(trigger)
        let newState = this.States |> List.find (fun x -> x.State = newCur)
        newState.Entry()
        this.Current <- newCur

let configure state = 
  { State = state; SuperState = None; Entry = (fun _ -> ()); Exit = (fun _ -> ()); Transitions = [] }

let permit trigger endState state =
  { state with Transitions = { Trigger = trigger; Func = (fun _ -> endState) }::state.Transitions }

let permitF trigger f state =
  { state with Transitions = { Trigger = trigger; Func = f }::state.Transitions }

let substateOf superState state =
  { state with SuperState = Some(superState) }

let onEntry f state = {state with Entry = f }
let onExit f state = {state with Exit = f }
