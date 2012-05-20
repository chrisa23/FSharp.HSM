module FSharp.HSM 

open System.Collections.Generic

type Transition<'state,'event> = 
  { Event: 'event 
    NextState: 'event -> obj -> 'state
    Guard: unit -> bool }
    
exception NoSuperState
   
type StateConfig<'state,'event> = 
  { State: 'state
    SuperState: 'state option
    AutoTransition: 'state option
    Entry: unit -> unit
    Exit: unit -> unit
    Transitions: Transition<'state,'event> list}
    member this.hasSuper = 
      match this.SuperState with
      | None -> false
      | Some _ -> true
    member this.getSuper = 
      match this.SuperState with
      | None -> Unchecked.defaultof<'state>
      | Some x -> x

exception NoTransition
exception AlreadyStarted

type StateMachine<'state,'event when 'state : equality and 'event :equality>(states:StateConfig<'state,'event> list ) = 
    //below is dangerous for enums/structs I believe...
    let mutable current = Unchecked.defaultof<'state>
    let find state = states |> List.find (fun x -> x.State = state)
    let rec findTransition event state = 
      match state.Transitions |> List.tryFind (fun x -> x.Event = event) with
      | None -> 
        match state.SuperState with //do we have a superstate?
        | None -> raise NoTransition//error
        | Some x -> findTransition event (find x)
      | Some x -> x
    let stateEvent = new Event<'state>()
    let rec getParents list state =
      let config = find state
      if config.hasSuper then 
        let super = config.getSuper
        getParents (super::list) super
      else list
    let rec exitSuper state limit = 
      match state.SuperState with
      | None -> ()
      | Some super -> 
        if limit <> Unchecked.defaultof<'state> && super = limit then ()
        else 
          let superConfig = find super
          superConfig.Exit()
          exitSuper superConfig limit 
    let rec transition currentState newState = 
      let currentStateConfig = find currentState
      let newStateConfig = states |> List.find (fun x -> x.State = newState)
      let isSelf = currentStateConfig.State = newStateConfig.State
      //has common parent (which becomes limit on exit super)
      let curParents = getParents [] currentState |> List.rev
      let newParents = getParents [] newState |> List.rev
      let commonParent = ref Unchecked.defaultof<'state>
      //fix this 
      let mutable foundParent = false
      for parent in curParents do
        if not foundParent && newParents |> List.exists (fun x -> x = parent) then
          commonParent := parent
          foundParent <- true//done...  STOP

      let moveToSub = newParents |> List.exists (fun x -> x = currentState)
      let shareSuper = !commonParent <> Unchecked.defaultof<'state>
//EXITS
      if (not moveToSub && not isSelf) || isSelf then
        currentStateConfig.Exit()
      //exit super states up to but not common parent
      exitSuper currentStateConfig !commonParent      
//ENTRY
      //enter parents below common Parents before newState, 
      //but not if we just autoed from there..
      if foundParent then
        let revParents = newParents |> List.rev 
        let index = revParents |> List.findIndex (fun x -> x = !commonParent)
        for parent in revParents |> Seq.skip (index + 1) do
          if parent <> currentState then (find parent).Entry()

      newStateConfig.Entry()

      current <- newState
      stateEvent.Trigger current

      match newStateConfig.AutoTransition with
      | None -> ()
      | Some x -> transition newState x
    ///Initializes the state machine with its initial state
    member this.Init state = 
        if not (current = Unchecked.defaultof<'state>) then raise AlreadyStarted
        current <- state
        let stateConfig = find current
        stateConfig.Entry()
        stateEvent.Trigger current
        match stateConfig.AutoTransition with
        | None -> ()
        | Some x -> transition state x
    member this.State with get() = current
    member this.StateChanged = stateEvent.Publish
    member this.IsIn (state:'state) = 
      if current = state then true else
        let cur = find current
        getParents [] current |> List.exists (fun x -> x = state)
    member this.Fire(event) = 
        let cur = find current
        let trans = findTransition event cur
        if trans.Guard() then transition current (trans.NextState event null)
    member this.Fire(event, data) = 
        let cur = find current
        let trans = findTransition event cur
        if trans.Guard() then transition current (trans.NextState event data)

let configure state = 
  { State = state; SuperState = None; 
    Entry = (fun _ -> ()); Exit = (fun _ -> ()); 
    AutoTransition = None; Transitions = [] }

let onEntry f state = {state with Entry = f }
let onExit f state = {state with Exit = f }

let substateOf superState state = { state with SuperState = Some(superState) }

///Sets an auto transition to a new state
let transitionTo substate state = { state with AutoTransition = Some(substate) }

///Sets a transition to a new state on an event (same state allows re-entry)
let on event endState state =
  { state with Transitions = { Event = event; NextState = (fun _ _ -> endState); Guard = (fun _ -> true) }::state.Transitions }
///Sets a guarded transition to a new state on an event (same state allows re-entry)
let onIf event guard endState state = 
  { state with Transitions = { Event = event; NextState = (fun _ _ -> endState); Guard = guard }::state.Transitions }
///Sets an event handler (with or without data) which returns the new state
let handle event f state = 
  { state with Transitions = { Event = event; NextState = f; Guard = (fun _ -> true) }::state.Transitions }
///Sets a guarded event handler (with or without data) which returns the new state
let handleIf event guard f state = 
  { state with Transitions = { Event = event; NextState = f; Guard = guard }::state.Transitions }

//todo
//let action event f state =
//let actionIf event guard f state =