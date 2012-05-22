module FSharp.HSM 

open System.Collections.Generic
open SymbolTables

exception NoSuperState
exception NoTransition
exception NotInitialized
exception AlreadyStarted

type Transition<'state,'event> = 
  { Event: 'event 
    NextState: 'event -> obj -> 'state
    Guard: unit -> bool }
    
type StateConfig<'state,'event> = 
  { State: 'state
    Entry: unit -> unit
    Exit: unit -> unit
    SuperState: 'state option
    Parents: 'state list
    AutoTransition: 'state option
    Transitions: Transition<'state,'event> list }

type StateMachine<'state,'event when 'state : equality and 'event :equality>(stateList:StateConfig<'state,'event> list) = 
    let mutable current = stateList.Head.State
    let mutable started = false
    let states = stateList |> List.map (fun x -> x.State) |> List.toArray
    let stateTable = SymbolTable states
    let configs = stateTable.Dictionary()
    let find state : StateConfig<'state,'event> = configs.[stateTable.Get state]
    let rec getParents results state =
      let currentConfig = stateList |> List.find (fun x -> x.State = state)
      if Option.isSome currentConfig.SuperState then 
        let super = Option.get currentConfig.SuperState
        getParents (super::results) super
      else results |> List.rev

    do 
      for stateConfig in stateList do
        configs.[stateTable.Get stateConfig.State] <- { stateConfig with Parents = getParents [] stateConfig.State }
    
    let rec findTransition event state = 
      match state.Transitions |> List.tryFind (fun x -> x.Event = event), state.SuperState with
      | None, None -> raise NoTransition
      | None, Some x -> findTransition event (find x)
      | Some x, _ -> x
    let stateEvent = new Event<'state>()

    let rec exitToCommonParent state limit = 
      match state.SuperState, limit with
      | None, _ -> ()
      | Some super, Some lim when super = lim -> ()
      | Some super, _  -> 
          let superConfig = find super
          superConfig.Exit()
          exitToCommonParent superConfig limit 

    let rec findCommon (list1:'state list) list2 = 
        if list1.IsEmpty then None else
        let restOfList1 = list1.Tail
        match list2 |> List.tryFind (fun x -> x = list1.Head), restOfList1 with
        | None, [] -> None
        | None, _ -> findCommon restOfList1 list2
        | Some x, _ -> Some(x)

    let rec transition currentState newState = 
      let currentStateConfig = find currentState
      let newStateConfig = find newState

      let isSelf = currentStateConfig.State = newStateConfig.State
      let moveToSub = not isSelf || newStateConfig.Parents |> List.exists (fun x -> x = currentState)
      let commonParent = if isSelf then None else findCommon currentStateConfig.Parents newStateConfig.Parents 
         
      if not moveToSub || isSelf then 
        currentStateConfig.Exit()

      exitToCommonParent currentStateConfig commonParent

      //enter parents below common Parents before newState, 
      //but not if we just autoed from there..
      match commonParent with
      | None -> ()
      | Some x -> //todo: optimize this
          let revParents = newStateConfig.Parents |> List.rev 
          let index = revParents |> List.findIndex (fun y -> y = x)
          for parent in revParents |> Seq.skip (index + 1) do
            if parent <> currentState then (find parent).Entry()

      current <- newState
      newStateConfig.Entry()
      stateEvent.Trigger newState

      match newStateConfig.AutoTransition with
      | None -> ()
      | Some x -> transition newState x

    ///Initializes the state machine with its initial state
    member this.Init state = 
      if started then raise AlreadyStarted else started <- true
      let stateConfig = find state
      current <- state
      stateConfig.Entry()
      stateEvent.Trigger current
      match stateConfig.AutoTransition with
      | None -> ()
      | Some x -> transition state x
    ///Gets the current state
    member this.State with get() = if not started then raise NotInitialized else current
    ///Raise on a state change
    member this.StateChanged = stateEvent.Publish
    ///Check whether in state or parent state
    member this.IsIn (state:'state) = 
      if not started then raise NotInitialized
      if current = state then true else
        (find current).Parents |> List.exists (fun x -> x = state)
    ///Fire an event without data
    member this.Fire(event) = 
      if not started then raise NotInitialized
      let cur = find current
      let trans = findTransition event cur
      if trans.Guard() then transition current (trans.NextState event null)
    ///Fire an event with data
    member this.Fire(event, data) = 
      let cur = find current
      let trans = findTransition event cur
      if trans.Guard() then transition current (trans.NextState event data)

///Sets up a new state config 
let configure state = 
  { State = state; Entry = (fun () -> ()); Exit = (fun () -> ()); 
    SuperState = None; Parents = []; AutoTransition = None; Transitions = [] }
///Sets an action on the entry of the state
let onEntry f state = {state with Entry = f }
///Sets an action on the exit of the state
let onExit f state = {state with Exit = f }
///Sets this state as a substate of the given state
let substateOf superState state = { state with SuperState = Some(superState) }
///Sets an auto transition to a new state
let transitionTo substate state = { state with AutoTransition = Some(substate) }
///Sets a transition to a new state on an event (same state allows re-entry)
let on event endState state =
  { state with Transitions = 
    { Event = event; NextState = (fun _ _ -> endState); Guard = (fun () -> true) }::state.Transitions }
///Sets a guarded transition to a new state on an event (same state allows re-entry)
let onIf event guard endState state = 
  { state with Transitions = 
    { Event = event; NextState = (fun _ _ -> endState); Guard = guard }::state.Transitions }
///Sets an event handler (with or without data) which returns the new state to transition to
let handle event f state = 
  { state with Transitions = 
    { Event = event; NextState = f; Guard = (fun () -> true) }::state.Transitions }
///Sets a guarded event handler (with or without data) which returns the new state
let handleIf event guard f state = 
  { state with Transitions = 
    { Event = event; NextState = f; Guard = guard }::state.Transitions }


//was adding actions based on Samek statechart example, but I don't think it is needed
//handle and handleIf take the event and an arg but must return a new state

////Sets an action on an event (can't be set in addition to a current state transition)
//let action event f state =

////Sets an action on an event
//let actionIf event guard f state =