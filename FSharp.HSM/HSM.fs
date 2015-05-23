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

type Transition<'state,'event> = 
  { Event: 'event 
    Guard: unit -> bool 
    NextState: obj -> 'state option }
    
type StateConfig<'state,'event when 'event:comparison> = 
  { State: 'state
    Entry: unit -> unit
    Exit: unit -> unit
    SuperState: 'state option
    Parents: 'state list
    AutoTransition: 'state option
    Transitions: Map<'event, Transition<'state,'event>> }

type internal StateMachine<'state,'event 
        when 'state : equality 
        and 'state : comparison 
        and 'event : comparison
        and 'event : equality> (stateList:StateConfig<'state,'event> list) = 
    let stateEvent = new Event<'state>()
    let mutable current = stateList.Head.State
    let mutable started = false
    let states = stateList |> List.map (fun x -> x.State) |> List.toArray
    
    let rec getParents state =
        let currentConfig = stateList |> List.find (fun x -> x.State = state)
        match currentConfig.SuperState with
        | None -> []
        | Some super -> super::(getParents super)

    let configs = 
        stateList 
        |> List.map (fun config -> 
            config.State, { config with Parents = getParents config.State } )
        |> Map.ofList

    let find state : StateConfig<'state,'event> = configs.[state]
    
    let rec findTransition event state = 
        match state.Transitions.TryFind event, state.SuperState with
        | None, None -> raise NoTransition
        | None, Some x -> findTransition event (find x)
        | Some x, _ -> x
    
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
        let currentStateConfig, newStateConfig = find currentState, find newState
      
        let isSelf = currentStateConfig.State = newStateConfig.State
        let moveToSub = not isSelf || newStateConfig.Parents |> List.exists (fun x -> x = currentState)
        let commonParent = 
            if isSelf then None 
            else findCommon currentStateConfig.Parents newStateConfig.Parents 
         
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

    interface IStateMachine<'state, 'event> with
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
            if trans.Guard() then 
                let nextState = trans.NextState null
                if Option.isSome nextState then 
                    transition current (Option.get nextState)
        ///Fire an event with data
        member this.Fire(event, data) = 
            if not started then raise NotInitialized
            let cur = find current
            let trans = findTransition event cur
            if trans.Guard() then
                let nextState = trans.NextState data
                if Option.isSome nextState then 
                    transition current (Option.get nextState)

let internal unit = fun () -> ()
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
    Entry = unit; 
    Exit = unit; 
    SuperState = None; 
    Parents = []; 
    AutoTransition = None; 
    Transitions = new Map<'event, Transition<'state,'event>>([]); }

///Sets am action on the entry of the state
let onEntry f state = {state with Entry = f }

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
