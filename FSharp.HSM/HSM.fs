module FSharp.HSM 

open System.Collections.Generic
open SymbolTables

type Transition<'state,'event> = 
  { Event: 'event 
    NextState: 'event -> obj -> 'state
    Guard: unit -> bool }
    //Action: unit -> unit 
    //ActionGuard: unit -> bool }

exception NoSuperState
    
type StateConfig<'state,'event> = 
  { State: 'state
    SuperState: 'state option//possibly remove with Parents field
    AutoTransition: 'state option
    Entry: unit -> unit
    Exit: unit -> unit
    Transitions: Transition<'state,'event> list 
    Parents: 'state list }
    member this.hasSuper = 
      match this.SuperState with
      | None -> false
      | Some _ -> true
    member this.getSuper = 
      match this.SuperState with
      | None -> raise NoSuperState
      | Some x -> x

exception NoTransition
exception NotInitialized
exception AlreadyStarted

type StateMachine<'state,'event when 'state : equality and 'event :equality>(stateList:StateConfig<'state,'event> list) = 
    let mutable current = stateList.Head.State
    let mutable started = false
    let states = stateList |> List.map (fun x -> x.State) 
    let stateTable = SymbolTable (states |> List.toArray)
    let configs = stateTable.Dictionary()
    let find state : StateConfig<'state,'event> = configs.[stateTable.Get state]
    
    let rec getParents (list:'state list) (state:'state) =
      let currentConfig = find state
      if currentConfig.hasSuper then 
        let super = currentConfig.getSuper
        getParents (super::list) super
      else list
    do 
      stateList |> List.iter (fun x -> configs.[stateTable.Get x.State] <- x )
      //set twice to add parents... parents uses list for lookup...
      stateList |> List.iter (fun x -> 
        configs.[stateTable.Get x.State] <- { x with Parents = getParents [] x.State |> List.rev } )
    
    let rec findTransition event state = 
      match state.Transitions |> List.tryFind (fun x -> x.Event = event), state.SuperState with
      | None, None -> raise NoTransition//error
      | None, Some x -> findTransition event (find x)
      | Some x, _ -> x
    let stateEvent = new Event<'state>()

    let rec exitSuper state limit = 
      match state.SuperState, limit with
      | None, _ -> ()
      | Some super, Some lim when super = lim -> ()
      | Some super, _  -> 
          let superConfig = find super
          superConfig.Exit()
          exitSuper superConfig limit 

    let rec findCommon (list1:'state list) list2 = 
        if list1.IsEmpty then None else
        let toCheck = list1.Head
        let restOfList1 = list1.Tail
        match list2 |> List.tryFind (fun x -> x = toCheck), restOfList1 with
        | None, [] -> None
        | None, _ -> findCommon restOfList1 list2
        | Some x, _ -> Some(x)

    let rec transition currentState newState = 
      let currentStateConfig = find currentState
      let newStateConfig = find newState

      let isSelf = currentStateConfig.State = newStateConfig.State

      //has common parent (which becomes limit on exit super)
      let commonParent = findCommon currentStateConfig.Parents newStateConfig.Parents 
      let mutable foundParent = false
      let moveToSub = newStateConfig.Parents |> List.exists (fun x -> x = currentState)
   
//EXITS
      if (not moveToSub && not isSelf) || isSelf then
        currentStateConfig.Exit()
      
      //exit super states up to but not common parent
      exitSuper currentStateConfig commonParent

//ENTRY
      //enter parents below common Parents before newState, 
      //but not if we just autoed from there..
      match commonParent with
      | None -> ()
      | Some x -> 
          let revParents = newStateConfig.Parents |> List.rev 
          let index = revParents |> List.findIndex (fun y -> y = x)
          for parent in revParents |> Seq.skip (index + 1) do
            if parent <> currentState then (find parent).Entry()

      newStateConfig.Entry()

      current <- newState
      stateEvent.Trigger newState

      match newStateConfig.AutoTransition with
      | None -> ()
      | Some x -> transition newState x

    ///Initializes the state machine with its initial state
    member this.Init state = 
      if started then raise AlreadyStarted
      started <- true
      current <- state
      let stateConfig = find current
      stateConfig.Entry()
      stateEvent.Trigger current
      match stateConfig.AutoTransition with
      | None -> ()
      | Some x -> transition state x
    ///Gets the current state
    member this.State 
      with get() = 
        if not started then raise NotInitialized
        current
    ///Raise on a state change
    member this.StateChanged = stateEvent.Publish
    ///Check whether in state or parent state
    member this.IsIn (state:'state) = 
      if not started then raise NotInitialized
      if current = state then true else
        getParents [] current |> List.exists (fun x -> x = state)
    ///Fire an event without data
    member this.Fire(event) = 
      if not started then raise NotInitialized
      let cur = find current
      let trans = findTransition event cur
      //if trans.ActionGuard() then trans.Action()
      if trans.Guard() then transition current (trans.NextState event null)
    ///Fire an event with data
    member this.Fire(event, data) = 
      let cur = find current
      let trans = findTransition event cur
      //if trans.ActionGuard() then trans.Action()
      if trans.Guard() then transition current (trans.NextState event data)

///Sets up a new state config 
let configure state = 
  { State = state; SuperState = None; Parents = [];
    Entry = (fun _ -> ()); Exit = (fun _ -> ()); 
    AutoTransition = None; Transitions = [] }

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
    { Event = event; NextState = (fun _ _ -> endState); Guard = (fun _ -> true) }::state.Transitions }
///Sets a guarded transition to a new state on an event (same state allows re-entry)
let onIf event guard endState state = 
  { state with Transitions = 
    { Event = event; NextState = (fun _ _ -> endState); Guard = guard }::state.Transitions }
///Sets an event handler (with or without data) which returns the new state
let handle event f state = 
  { state with Transitions = { Event = event; NextState = f; Guard = (fun _ -> true) }::state.Transitions }
///Sets a guarded event handler (with or without data) which returns the new state
let handleIf event guard f state = 
  { state with Transitions = { Event = event; NextState = f; Guard = guard }::state.Transitions }

////Sets an action on an event (can't be set in addition to a current state transition)
//let action event f state =
////todo: find if we have a transition and update, if not, 
//  { state with Transitions = 
//    { Event = event; NextState = (fun _ _ -> state.State); Guard =  (fun _ -> false); 
//      Action = f; ActionGuard = (fun _ -> true) }::state.Transitions }
//
////Sets an action on an event
//let actionIf event guard f state =
//  { state with Transitions = 
//    { Event = event; NextState = (fun _ _ -> state.State); Guard =  (fun _ -> false); 
//      Action = f; ActionGuard = guard }::state.Transitions }