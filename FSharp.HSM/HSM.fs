module FSharp.HSM

type IStateMachine<'state, 'event, 'output, 'data> =
    abstract member StateChanged: IEvent<'state>
    abstract member EventRaised: IEvent<'output>
    abstract member Init: 'state -> unit
    abstract member State: 'state option
    abstract member Permitted: 'event []
    abstract member CanFire: 'event -> bool
    abstract member IsIn: 'state -> bool
    abstract member Fire: 'event -> unit
    abstract member Fire: 'event * 'data -> unit

///Holds transition information for state
type Transition<'state, 'event, 'output, 'data> =
    { Event: 'event
      Guard: unit -> bool
      Raises: 'output option
      NextState: 'data option -> 'state option }

///Holds details of state
type StateConfig<'state, 'event, 'output, 'data when 'event: comparison> =
    { State: 'state
      Enter: unit -> unit
      Exit: unit -> unit
      SuperState: 'state option
      Parents: StateConfig<'state, 'event, 'output, 'data> list
      AutoTransition: 'state option
      Transitions: Map<'event, Transition<'state, 'event, 'output, 'data>>
      EnterRaises: 'output list
      ExitRaises: 'output list }

module internal Core =

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
        | Some super ->
            (find stateList super)
            :: (getParents stateList super)

    ///Create a new list with all states and their parents
    let compileParents stateList =
        stateList
        |> List.map
            (fun x ->
                { x with
                      Parents = getParents stateList x.State })

    ///Create state definition map
    let createStateMap stateList =
        stateList
        |> compileParents
        |> List.map (fun config -> config.State, config)
        |> Map.ofList

    ///Find a transition from a state and its parents
    let rec findTransition event state parents =
        match state.Transitions.TryFind event, parents with
        | None, [] -> None
        | None, h :: t -> findTransition event h t
        | Some x, _ -> Some x

    ///Try and find a common state among state lists
    let rec findCommon
        (list1: StateConfig<'state, 'event, 'output, 'data> list)
        (list2: StateConfig<'state, 'event, 'output, 'data> list)
        =
        if list1.IsEmpty || list2.IsEmpty then
            None
        else
            let h::t = list2

            match tryFind list1 h.State, t with
            | None, [] -> None
            | None, _ -> findCommon list1 t
            | Some x, _ -> Some(x)

    //Helpers
    let exit f state =
        state.Exit()
        List.iter f state.ExitRaises

    let enter f state =
        state.Enter()
        List.iter f state.EnterRaises

    let getTakeWhile list1 list2 =
        match findCommon list1 list2 with
        | None -> (fun x -> true)
        | Some x -> (fun y -> x.State <> y.State)

    let exitParents f takeWhile currentParents =
        currentParents
        |> List.takeWhile takeWhile
        |> List.iter (exit f)

    let enterParents f takeWhile newParents =
        newParents
        |> List.takeWhile takeWhile
        |> List.rev
        |> List.iter (enter f)

    let exitState f current newState =
        let takeWhile =
            getTakeWhile current.Parents newState.Parents

        exitParents f takeWhile current.Parents

    let enterState f current newState =
        let takeWhile =
            getTakeWhile (current :: current.Parents) newState.Parents

        enterParents f takeWhile newState.Parents

    let getTransitions state =
        state.Transitions |> Map.toArray |> Array.map fst

    type internal StateMachine<'state, 'event, 'output, 'data when 'state: equality and 'state: comparison and 'event: equality and 'event: comparison>(stateList: StateConfig<'state, 'event, 'output, 'data> list) =

        let stateEvent = new Event<'state>()
        let outputEvent = new Event<'output>()

        let mutable current =
            Unchecked.defaultof<StateConfig<'state, 'event, 'output, 'data>>

        let mutable started = false

        let configs = stateList |> createStateMap

        let trigger x = outputEvent.Trigger x

        let rec transition newState =
            let newStateConfig = configs.[newState]

            let isSub =
                tryFind newStateConfig.Parents current.State
                |> Option.isSome

            //If we are transitioning to ourselves exit, but don't exit if we are going to a sub state
            if current.State = newStateConfig.State || not isSub then
                current.Exit()

            List.iter trigger current.ExitRaises

            exitState trigger current newStateConfig
            enterState trigger current newStateConfig

            newStateConfig.Enter()

            current <- newStateConfig

            stateEvent.Trigger newState

            List.iter trigger newStateConfig.EnterRaises

            match newStateConfig.AutoTransition with
            | None -> ()
            | Some x -> transition x

        let tryTransition data trans =
            if trans.Guard() then
                Option.iter trigger trans.Raises
                Option.iter transition (trans.NextState data)
        //else look for another transition deeper...

        interface IStateMachine<'state, 'event, 'output, 'data> with
            ///Initializes the state machine with its initial state
            member this.Init state =
                started <- true
                current <- configs.[state]

                current.Parents
                |> List.rev
                |> List.iter (enter trigger)

                current.Enter()

                stateEvent.Trigger current.State

                match current.AutoTransition with
                | None -> ()
                | Some x -> transition x
            ///Gets the current state
            member this.State =
                if not started then
                    None
                else
                    Some current.State
            ///Raise on a state change
            member this.StateChanged = stateEvent.Publish
            //Raise an output event
            member this.EventRaised = outputEvent.Publish

            member this.Permitted =
                if not started then
                    [||]
                else
                    let parents =
                        Array.collect getTransitions (current.Parents |> List.toArray)

                    let curTrans = current |> getTransitions
                    Array.append parents curTrans

            member this.CanFire event =
                if not started then
                    false
                else
                    Array.contains event (this :> IStateMachine<_, 'event, _, _>).Permitted

            ///Check whether in state or parent state
            member this.IsIn(state: 'state) =
                if not started then
                    false
                else if current.State = state then
                    true
                else
                    current.Parents
                    |> List.exists (fun x -> x.State = state)
            ///Fire an event without data
            member this.Fire(event) =
                if not started then
                    ()
                else
                    let trans =
                        findTransition event current current.Parents

                    Option.iter (tryTransition None) trans

            ///Fire an event with data
            member this.Fire(event, data) =
                if not started then
                    ()
                else
                    let trans =
                        findTransition event current current.Parents

                    Option.iter (tryTransition (Some data)) trans

let private noop = fun () -> ()
let private alwaysTrue = fun () -> true

///Create a state machine from configuration list
let create (stateList: StateConfig<'state, 'event, 'output, 'data> list) =
    (Core.StateMachine<'state, 'event, 'output, 'data>(stateList)) :> IStateMachine<'state, 'event, 'output, 'data>

//###################################################################################
//Builder DSL
//Takes advantage of F# record's 'with' functionality to allow chaining of
//configurations
//###################################################################################

///Sets up a new state config
let configure state =
    { State = state
      Enter = noop
      Exit = noop
      SuperState = None
      Parents = []
      AutoTransition = None
      EnterRaises = []
      ExitRaises = []
      Transitions = new Map<'event, Transition<'state, 'event, 'output, 'data>>([]) }

///Sets am action on the entry of the state
let onEntry f state = { state with Enter = state.Enter >> f }

///Sets an action on the exit of the state
let onExit f state = { state with Exit = state.Exit >> f }

///Sets this state as a substate of the given state
let substateOf superState state =
    { state with
          SuperState = Some(superState) }

///Sets an auto transition to a new state
let transitionTo substate state =
    { state with
          AutoTransition = Some(substate) }

///Raises an output event
let enterRaises output state =
    { state with
          EnterRaises = output :: state.EnterRaises }

///Raises an output event
let exitRaises output state =
    { state with
          ExitRaises = output :: state.ExitRaises }

///Sets a transition to a new state on an event (same state allows re-entry)
let on event endState state =
    let transition =
        { Event = event
          NextState = (fun _ -> Some(endState))
          Guard = alwaysTrue
          Raises = None }

    { state with
          Transitions = state.Transitions.Add(event, transition) }

///Sets a guarded transition to a new state on an event (same state allows re-entry)
let onIf event guard endState state =
    let transition =
        { Event = event
          NextState = (fun _ -> Some(endState))
          Guard = guard
          Raises = None }

    { state with
          Transitions = state.Transitions.Add(event, transition) }

///Sets an event handler (with or without data) which returns the new state to transition to
let handle event f state =
    let trans =
        { Event = event
          NextState = f
          Guard = alwaysTrue
          Raises = None }

    { state with
          Transitions = state.Transitions.Add(event, trans) }

///Sets a guarded event handler (with or without data) which returns the new state
let handleIf event guard f state =
    { state with
          Transitions =
              state.Transitions.Add(
                  event,
                  { Event = event
                    NextState = f
                    Guard = guard
                    Raises = None }
              ) }

//Transitions withRaise

///Sets a transition to a new state on an event (same state allows re-entry)
let onWithRaise event endState output state =
    let transition =
        { Event = event
          NextState = (fun _ -> Some(endState))
          Guard = alwaysTrue
          Raises = Some output }

    { state with
          Transitions = state.Transitions.Add(event, transition) }

///Sets a guarded transition to a new state on an event (same state allows re-entry)
let onIfWithRaise event guard endState output state =
    let transition =
        { Event = event
          NextState = (fun _ -> Some(endState))
          Guard = guard
          Raises = Some output }

    { state with
          Transitions = state.Transitions.Add(event, transition) }

///Sets an event handler (with or without data) which returns the new state to transition to
let handleWithRaise event f output state =
    let trans =
        { Event = event
          NextState = f
          Guard = alwaysTrue
          Raises = Some output }

    { state with
          Transitions = state.Transitions.Add(event, trans) }

///Sets a guarded event handler (with or without data) which returns the new state
let handleIfWithRaise event guard f output state =
    { state with
          Transitions =
              state.Transitions.Add(
                  event,
                  { Event = event
                    NextState = f
                    Guard = guard
                    Raises = Some output }
              ) }

///Connect output events from one SM to input of another
//let connect (hsm1: IStateMachine<_, _, 'output>) (hsm2: IStateMachine<_, 'output, _>) =
//    hsm1.EventRaised.Add(fun output -> hsm2.Fire output)
