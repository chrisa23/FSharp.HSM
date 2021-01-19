namespace FSharp.HSM.Tests  

open FSharp.HSM
open System
open NUnit.Framework
open FsUnit
open TestHelpers
open System.Timers

module PELICAN = 

    type State = 
        | Off
        | Operational
        | VehiclesEnabled
        | VehiclesGreen
        | VehiclesGreenInt
        | VehiclesYellow
        | PedestriansEnabled
        | PedestriansWalk
        | PedestriansFlash

    type Event =
        | Timeout
        | Off
        | PedestrianWaiting

    type VehicleSignal = 
        | Red
        | Yellow
        | Green

    type PedestrianSignal =
        | Walk
        | Blank
        | DontWalk

    type Output = 
        | Vehicle of VehicleSignal
        | Pedestrian of PedestrianSignal
    
    type PelicanSignal() = 

        let mutable hsm = Unchecked.defaultof<IStateMachine<State,Event, Output, unit>>

        let vehicleSignal = Event<VehicleSignal>()
        let pedestrianSignal = Event<PedestrianSignal>()

        let runTimer x = 
            async { 
                do! Async.Sleep (int( x * 1000.)) 
                printfn "Timeout %A" DateTime.Now
                hsm.Fire Timeout
                } |> Async.Start
        
        let setTimer x = (fun () -> runTimer x)

        let shutdown() = printfn "shutdown" 

        let mutable isPedestrianWaiting = false
        let flashCount = ref 0

        let signalVehicles (state: VehicleSignal) = 
            printfn "%A" state
            vehicleSignal.Trigger state

        let signalPedestrians (state: PedestrianSignal) = 
            printfn "%A" state
            pedestrianSignal.Trigger state

        let pedestrianWaitingOff() = 
            isPedestrianWaiting <- false

        let handleWaitingOnGreen _ =
            isPedestrianWaiting <- true
            None

        let handleTimeoutOnGreen _ =
            if isPedestrianWaiting then Some VehiclesYellow 
            else Some VehiclesGreenInt

        let setFlashCount() = flashCount := 11

        let timeoutFlashing _ = 
            decr flashCount
            printfn "flashCount: %i" !flashCount
            match !flashCount with
            | 0 -> Some VehiclesEnabled
            | x when (x % 2) = 1 -> 
                signalPedestrians Walk
                Some PedestriansFlash
            | _ -> 
                signalPedestrians Blank
                Some PedestriansFlash

        let handleEvent output =
            match output with 
            | Vehicle v -> signalVehicles v
            | Pedestrian p -> signalPedestrians p

        do hsm <-
            [
                configure State.Off
                    |> onEntry shutdown
                configure Operational
                    |> onEntry (fun () -> printfn "Operational %A" DateTime.Now)
                    |> on Event.Off State.Off
                    |> transitionTo VehiclesEnabled
                configure VehiclesEnabled
                    |> substateOf Operational
                    |> enterRaises (Pedestrian(PedestrianSignal.DontWalk))
                    |> transitionTo VehiclesGreen
                configure VehiclesGreen
                    |> substateOf VehiclesEnabled
                    |> onEntry (setTimer 10.)
                    |> onEntry pedestrianWaitingOff
                    |> enterRaises (Vehicle(VehicleSignal.Green))
                    |> handle PedestrianWaiting handleWaitingOnGreen
                    |> handle Timeout handleTimeoutOnGreen
                configure VehiclesGreenInt
                    |> substateOf VehiclesEnabled
                    |> on PedestrianWaiting VehiclesYellow
                configure VehiclesYellow
                    |> substateOf VehiclesEnabled
                    |> onEntry (setTimer 4.)
                    |> enterRaises (Vehicle(VehicleSignal.Yellow))
                    |> on Timeout PedestriansEnabled
                configure PedestriansEnabled
                    |> substateOf Operational
                    |> transitionTo PedestriansWalk
                    |> enterRaises (Vehicle(VehicleSignal.Red))
                configure PedestriansWalk
                    |> substateOf PedestriansEnabled
                    |> onEntry (setTimer 10.)
                    |> enterRaises (Pedestrian(PedestrianSignal.Walk))
                    |> onExit setFlashCount
                    |> on Timeout PedestriansFlash
                configure PedestriansFlash
                    |> substateOf PedestriansEnabled
                    |> onEntry (setTimer 0.5)
                    |> handle Timeout timeoutFlashing

            ]
            |> create
           hsm.EventRaised.Add handleEvent

        member this.Start() = hsm.Init Operational
        member this.Stop() = hsm.Fire Event.Off
        member this.Walk() = hsm.Fire PedestrianWaiting

        member this.VehicleSignal = vehicleSignal.Publish
        member this.PedestrianSignal = pedestrianSignal.Publish

        member this.State = hsm.StateChanged


    [<Test>]
    let run() =
        let signal = PelicanSignal()
        signal.State.Add(fun x -> printfn "%A" x)
        signal.Start()
        Async.Sleep 5000 |> Async.RunSynchronously
        signal.Walk()
        Async.Sleep 35000 |> Async.RunSynchronously
        signal.Stop()



