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

    
    
    type PelicanSignal() = 

        let mutable hsm = Unchecked.defaultof<IStateMachine<State,Event>>

        let vehicleSignal = Event<VehicleSignal>()
        let vehicleSignal = Event<PedestrianSignal>()

        let runTimer x = 
            async { 
                do! Async.Sleep (int( x * 1000.)) 
                printfn "Timeout %A" DateTime.Now
                hsm.Fire Timeout
                } |> Async.Start
        
        let setTimer x = (fun () -> runTimer x)

        //let killTimer() = ()//timer.Dispose() 

        let shutdown() = printfn "shutdown" 

        let mutable isPedestrianWaiting = false
        let flashCount = ref 0

        let signalVehicles (state: VehicleSignal) = printfn "%A" state

        let signalPedestrians (state: PedestrianSignal) = printfn "%A" state

        let dontWalk() = signalPedestrians DontWalk
        let walk() = signalPedestrians Walk
        let blank() = signalPedestrians Blank

        let green() = 
            isPedestrianWaiting <- false
            signalVehicles Green
        let yellow() = signalVehicles Yellow
        let red() = signalVehicles Red

        let handleWaitingOnGreen _ =
            isPedestrianWaiting <- true
            None

        let handleTimeoutOnGreen _ =
            if isPedestrianWaiting 
            then Some VehiclesYellow 
            else Some VehiclesGreenInt

        let setFlashCount() = flashCount := 7

        let timeoutFlashing _ = 
            decr flashCount
            printfn "flashCount: %i" !flashCount
            match !flashCount with
            | 0 -> Some VehiclesEnabled
            | x when (x % 2) = 1 -> 
                walk()
                Some PedestriansFlash
            | _ -> 
                blank()
                Some PedestriansFlash
        
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
                    |> onEntry dontWalk
                    |> transitionTo VehiclesGreen
                configure VehiclesGreen
                    |> substateOf VehiclesEnabled
                    |> onEntry (setTimer 10.)
                    |> onEntry green
                    //|> onExit killTimer
                    |> handle PedestrianWaiting handleWaitingOnGreen
                    |> handle Timeout handleTimeoutOnGreen
                configure VehiclesGreenInt
                    |> substateOf VehiclesEnabled
                    |> on PedestrianWaiting VehiclesYellow
                configure VehiclesYellow
                    |> substateOf VehiclesEnabled
                    |> onEntry (setTimer 3.)
                    |> onEntry yellow
                    //|> onExit killTimer
                    |> on Timeout PedestriansEnabled
                configure PedestriansEnabled
                    |> substateOf Operational
                    |> transitionTo PedestriansWalk
                    |> onEntry red
                configure PedestriansWalk
                    |> substateOf PedestriansEnabled
                    |> onEntry (setTimer 10.)
                    |> onEntry walk
                    //|> onExit killTimer
                    |> onExit setFlashCount
                    |> on Timeout PedestriansFlash
                configure PedestriansFlash
                    |> substateOf PedestriansEnabled
                    |> onEntry (setTimer 0.5)
                    //|> onExit killTimer
                    |> handle Timeout timeoutFlashing

            ]
            |> create


        member this.Start() = hsm.Init Operational
        member this.Stop() = hsm.Fire Event.Off
        member this.Walk() = hsm.Fire PedestrianWaiting
        


    [<Test>]
    let run() =
        let signal = PelicanSignal()
        signal.Start()
        Async.Sleep 5000 |> Async.RunSynchronously
        signal.Walk()
        Async.Sleep 35000 |> Async.RunSynchronously
        signal.Stop()



