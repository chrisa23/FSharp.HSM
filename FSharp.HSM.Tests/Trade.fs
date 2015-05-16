namespace AutomatonTests  

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

    type Trade() = 
        let send event p = 
            printfn "Sending orders for trade"
            Some(InMarket)
        let onFill event p = 
            printfn "Got fill -> InTrade"
            Some(InTrade)
        let increase event p = 
            printfn "Increase position"
            Some(InTrade)
        let decrease event p = 
            printfn "Decrease position"
            Some(InTrade)
        let cancel event p = 
            printfn "Cancel"
            Some(Exiting)
        let exit event p = 
            printfn "Exit"
            Some(Exiting)
        let onExitFill event p = 
            printfn "Got fill -> Exited"
            Some(Completed)
        let state = 
          create
            [ configure NotStarted
                |> handle Start send //handle
              configure InMarket
                |> handle Fill onFill //handle 
                |> handle Exit cancel
              configure InTrade
                |> handle Increase increase
                |> handle Decrease decrease
                |> handle Exit exit
              configure Exiting
                |> handle Fill onExitFill
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

      fireW trade Fill { Price = 3.0; Quantity = 2; Time = 3L; }
      
      check trade [InTrade] [InMarket;  Completed; ]

      fireW trade Increase 1
      
      check trade [InTrade] [InMarket;  Completed; ]

      fireW trade Decrease 1
      
      check trade [InTrade] [InMarket;  Completed; ]

      fire trade Exit
      
      check trade [Exiting] [InMarket;  InTrade; Completed; ]

      fireW trade Fill  { Price = 3.0; Quantity = 2; Time = 3L; }

      check trade [Completed] [InMarket;  InTrade; Exiting]

