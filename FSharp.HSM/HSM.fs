module FSharp.HSM 

exception NoTransition
exception NotInitialized
exception AlreadyStarted

type IStateMachine<'state,'event> =
    abstract member StateChanged: IEvent<'state>
    abstract member Init: 'state -> unit
    abstract member State: 'state with get
    abstract member IsIn: 'state -> bool
    abstract member Fire: 'event -> unit
    abstract member Fire: 'event * obj -> unit

///Holds transition information for state
type Transition<'state,'event> = 
  { Event: 'event 
    Guard: unit -> bool 
    NextState: obj -> 'state option }

///Holds details of state
type StateConfig<'state,'event when 'event:comparison> = 
  { State: 'state
    Enter: unit -> unit
    Exit: unit -> unit
    SuperState: 'state option
    Parents: StateConfig<'state,'event> list
    AutoTransition: 'state option
    Transitions: Map<'event, Transition<'state,'event>> }

///Find the state definition from a list
let find configList state =
    List.find (fun x -> x.State = state) configList

///Try find the state definition    
let tryFind configList state =
    List.tryFind (fun x -> x.State = state) configList

///Get the parents of a state    
let rec getParents stateList state =
    let currentConfig = find stateList state
    match currentConfig.SuperState with
    | None -> []
    | Some super -> (find stateList super)::(getParents stateList super)

///Create a new list with all states and their parents
let compileParents stateList =
    stateList
    |> List.map (fun x -> {x with Parents = getParents stateList x.State })

///Create state definitaion map
let createStateMap stateList =
    stateList 
    |> compileParents
    |> List.map (fun config -> config.State, config )
    |> Map.ofList

///Find a transition from a state and its parents
let rec findTransition event state parents = 
    match state.Transitions.TryFind event, parents with
    | None, [] -> raise NoTransition
    | None, h::t -> findTransition event h t
    | Some x, _ -> x

///Try and find a common state among state lists
let rec findCommon (list1:StateConfig<'state,'event> list) (list2:StateConfig<'state,'event> list) = 
    if list1.IsEmpty || list2.IsEmpty then None else
    let h::t = list2
    match tryFind list1 h.State, t with
    | None, [] -> None
    | None, _ -> findCommon list1 t
    | Some x, _ -> Some(x)

//Helpers
let exit state = state.Exit()
let enter state = state.Enter()

///Do exit/enter
let exitEnter takeWhile currentParents newParents =
    currentParents 
    |> Seq.takeWhile takeWhile
    |> Seq.iter exit

    newParents 
    |> Seq.takeWhile takeWhile
    |> Seq.toList
    |> List.rev 
    |> List.iter enter
  
///Exit from current parents and enter new parents, up to first common parent
let exitThenEnterParents currentParents newParents =
    let takeWhile = 
        match findCommon currentParents newParents  with
        | None -> (fun x -> true)
        | Some x -> (fun y -> x.State <> y.State)
    exitEnter takeWhile currentParents newParents


type internal StateMachine<'state,'event 
        when 'state : equality and 'state : comparison 
        and 'event : equality and 'event : comparison> (stateList:StateConfig<'state,'event> list) = 
    
    let stateEvent = new Event<'state>()
    let mutable current = Unchecked.defaultof<StateConfig<'state,'event>>
    let mutable started = false

    let configs = stateList |> createStateMap

    let rec transition newState = 
        let newStateConfig = configs.[newState]
             
        if current.State <> newStateConfig.State then current.Exit()

        exitThenEnterParents current.Parents newStateConfig.Parents
            
        current <- newStateConfig
        newStateConfig.Enter()
        
        match newStateConfig.AutoTransition with
        | None -> stateEvent.Trigger newState
        | Some x -> transition x

    interface IStateMachine<'state, 'event> with
        ///Initializes the state machine with its initial state
        member this.Init state = 
            if started then raise AlreadyStarted else started <- true
            current <- configs.[state]
            current.Parents |> List.rev |> List.iter enter
            current.Enter()
            
            match current.AutoTransition with
            | None -> stateEvent.Trigger current.State
            | Some x -> transition x
        ///Gets the current state
        member this.State with get() = if not started then raise NotInitialized else current.State
        ///Raise on a state change
        member this.StateChanged = stateEvent.Publish
        ///Check whether in state or parent state
        member this.IsIn (state:'state) = 
            if not started then raise NotInitialized
            if current.State = state then true else
                current.Parents |> List.exists (fun x -> x.State = state)
        ///Fire an event without data
        member this.Fire(event) = 
            if not started then raise NotInitialized
            let trans = findTransition event current current.Parents
            if trans.Guard() then 
                Option.iter transition (trans.NextState null)
        ///Fire an event with data
        member this.Fire(event, data) = 
            if not started then raise NotInitialized
            let trans = findTransition event current current.Parents
            if trans.Guard() then
                Option.iter transition (trans.NextState data)

let internal empty = fun () -> ()
let internal tru = fun () -> true

///Create a state machine from configuration list
let create(stateList:StateConfig<'state,'event> list) = 
    (StateMachine<'state,'event>(stateList)) :> IStateMachine<'state,'event>

//###################################################################################
//Builder DSL
//Takes advantage of F# record's 'with' functionality to allow chaining of 
//configurations
//###################################################################################

///Sets up a new state config 
let configure state = 
  { State = state; 
    Enter = empty; 
    Exit = empty; 
    SuperState = None; 
    Parents = []; 
    AutoTransition = None; 
    Transitions = new Map<'event, Transition<'state,'event>>([]); }

///Sets am action on the entry of the state
let onEntry f state = {state with Enter = f }

///Sets an action on the exit of the state
let onExit f state = {state with Exit = f }

///Sets this state as a substate of the given state
let substateOf superState state = { state with SuperState = Some(superState) }

///Sets an auto transition to a new state
let transitionTo substate state = { state with AutoTransition = Some(substate) }

///Sets a transition to a new state on an event (same state allows re-entry)
let on event endState state =
    let transition = { Event = event; NextState = (fun _ -> Some(endState)); Guard = tru }
    { state with Transitions = state.Transitions.Add (event, transition) }

///Sets a guarded transition to a new state on an event (same state allows re-entry)
let onIf event guard endState state = 
    let transition = { Event = event; NextState = (fun _ -> Some(endState)); Guard = guard }
    { state with Transitions = state.Transitions.Add (event, transition ) }

///Sets an event handler (with or without data) which returns the new state to transition to
let handle event f state = 
    let trans = { Event = event; NextState = f; Guard = tru }
    { state with Transitions = state.Transitions.Add (event, trans) }

///Sets a guarded event handler (with or without data) which returns the new state
let handleIf event guard f state = 
  { state with Transitions = state.Transitions.Add (event,
    { Event = event; NextState = f; Guard = guard }) }
