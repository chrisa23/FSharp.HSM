module Automaton 

open System.Collections.Generic

//how do I make these internal?
type Transition<'state,'event> = 
  { Event: 'event 
    State: 'state 
    Guard: unit -> bool }

exception NoSuperState
   
type StateConfig<'state,'event> = 
  { State: 'state
    SuperState: 'state option
    AutoTransition: 'state option
    Entry: unit -> unit//?? 'state -> 'event -> unit //old State first?
    Exit: unit -> unit//?? 'state ->unit //newState
    Reentry: bool
    Transitions: Transition<'state,'event> list}
    member this.hasSuper = match this.SuperState with
                           | None -> false
                           | Some _ -> true
    member this.getSuper = match this.SuperState with
                           | None -> raise NoSuperState
                           | Some x -> x
exception NoTransition

type StateMachine<'state,'event when 'state : equality and 'event :equality>(states:StateConfig<'state,'event> list ) = 
    let mutable current = Unchecked.defaultof<'state>
    let find state = states |> List.find (fun x -> x.State = state)
    let rec findTransition event state = 
        match state.Transitions |> List.tryFind (fun x -> x.Event = event) with
        | None -> 
            match state.SuperState with //do we have a superstate?
            | None -> raise NoTransition//error
            | Some x -> findTransition event (find x)
        | Some x ->   x
    let stateEvent = new Event<'state>()
    let rec transition currentState newState = 
      let currentStateConfig = find currentState
      let newStateConfig = states |> List.find (fun x -> x.State = newState)

      let isSelf = currentStateConfig.State = newStateConfig.State
      let moveToSub = newStateConfig.hasSuper && newStateConfig.getSuper = currentStateConfig.State
      let curIsSub = currentStateConfig.hasSuper
      let shareSuper = currentStateConfig.hasSuper && newStateConfig.hasSuper && currentStateConfig.getSuper =  newStateConfig.getSuper
      let newIsSuper = currentStateConfig.hasSuper && currentStateConfig.getSuper = newStateConfig.State

      let rec exitSuper state = 
        match state.SuperState with
        | None -> ()
        | Some super -> 
            let superConfig = find super
            superConfig.Exit()
            exitSuper superConfig
      let mutable calledExit = false
//EXITS
      if (not moveToSub && not isSelf) 
          || (isSelf && currentStateConfig.Reentry)  
      then  
          currentStateConfig.Exit()
          calledExit <- true

      //todo:  fix enter/exit logic...

            //do we need to call exit up the chain?
            //if we are in a substate and not transition to super
            //call the super's exit
      if curIsSub && not shareSuper then//   //broke for multiple hierarchies
      //are we leaving any substate 
          exitSuper currentStateConfig         


//ENTRY
      //if not transition from sub to super
      if (not(newIsSuper) && not(isSelf)) 
         || (isSelf && currentStateConfig.Reentry)  then 
          newStateConfig.Entry()

      current <- newState
      stateEvent.Trigger current
//
      match newStateConfig.AutoTransition with
      | None -> ()
      | Some x -> transition newState x



    member this.Init state = 
        current <- state
        let stateConfig = find current
        stateConfig.Entry()
        stateEvent.Trigger current
        //if not started, transition to init state, calling Enter
        match stateConfig.AutoTransition with
        | None -> ()
        | Some x -> transition state x
    member this.State with get() = current
    member this.StateChanged = stateEvent.Publish
    member this.IsIn (state:'state) = 
      if current = state then true else
        let cur = find current
        cur.hasSuper && cur.getSuper = state
    member this.Fire event = 
        let cur = find current
        let trans = findTransition event cur
        if trans.Guard() then transition current trans.State

let configure state = 
  { State = state; SuperState = None; Entry = (fun _ -> ()); Exit = (fun _ -> ()); 
    Reentry = false; AutoTransition = None; Transitions = [] }

let guard f state = { state with Guard = f }

let transitionTo substate state = { state with AutoTransition = Some(substate) }

let substateOf superState state = { state with SuperState = Some(superState) }

let onEntry (f: unit->unit) state = {state with Entry = f }

let onExit f state = {state with Exit = f }

let permit event endState state =
  { state with Transitions = { Event = event; State = endState; Guard = (fun _ -> true) }::state.Transitions }

let permitIf event endState guard state = { state with Transitions = { Event = event;State = endState; Guard = guard }::state.Transitions }

//let on event state
//let onIf event state

let permitReentry event state = 
  { state with Reentry = true; Transitions = { Event = event; State = state.State; Guard = (fun _ -> true) }::state.Transitions }

let permitReentryIf event guard state = 
  { state with Reentry = true; Transitions = { Event = event; State = state.State; Guard = guard }::state.Transitions }


//todo:
//ignore
//unhandled handler
//todo: implement action on transition with guard