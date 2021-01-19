namespace FSharp.HSM.Tests  

open FSharp.HSM
open System
open NUnit.Framework
open FsUnit
open TestHelpers

module TradeTest =
    
    type State = 
     | NotStarted
     | InMarket//markettrade delay or limit trades
     | InTrade
     | Exiting
     | Completed

    type Trigger =
     | Start 
     | Fill
     | Exit
     | Increase 
     | Decrease 

    type Fill = { Price: float; Quantity: int; Time: int64; }

    type Data = 
        | Int of int
        | Fill of Fill

    type Trade() = 
        let send p = 
            printfn "Sending orders for trade"
            Some(InMarket)
        let onFill p = 
            printfn "Got fill -> InTrade"
            Some(InTrade)
        let increase p = 
            printfn "Increase position"
            Some(InTrade)
        let decrease p = 
            printfn "Decrease position"
            Some(InTrade)
        let cancel p = 
            printfn "Cancel"
            Some(Exiting)
        let exit p = 
            printfn "Exit"
            Some(Exiting)
        let onExitFill p = 
            printfn "Got fill -> Exited"
            Some(Completed)
        let state = 
          create
            [ configure NotStarted
                |> handle Start send //handle
              configure InMarket
                |> handle Trigger.Fill onFill //handle 
                |> handle Exit cancel
              configure InTrade
                |> handle Increase increase
                |> handle Decrease decrease
                |> handle Exit exit
              configure Exiting
                |> handle Trigger.Fill onExitFill
              configure Completed 
                |> onEntry (fun _ -> printfn "calcPL")] 
        do 
          state.Init NotStarted
        member this.State with get() = state

    let check trade trueStates falseStates  = 
        trueStates |> List.iter (isInState trade)
        falseStates |> List.iter (isNotInState trade)

    [<Test>]
    let ``Trade1``() =
      let trade =(new Trade()).State
      attachShow trade

      fire trade Start

      check trade [InMarket] [ Completed ]

      fireW trade Trigger.Fill (Data.Fill ({ Price = 3.0; Quantity = 2; Time = 3L; }))
      
      check trade [InTrade] [InMarket;  Completed; ]

      fireW trade Increase (Data.Int( 1))
      
      check trade [InTrade] [InMarket;  Completed; ]

      fireW trade Decrease (Data.Int( 1))
      
      check trade [InTrade] [InMarket;  Completed; ]

      fire trade Exit
      
      check trade [Exiting] [InMarket;  InTrade; Completed; ]

      fireW trade Trigger.Fill (Data.Fill ({ Price = 3.0; Quantity = 2; Time = 3L; }))

      check trade [Completed] [InMarket;  InTrade; Exiting]

