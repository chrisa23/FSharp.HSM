namespace FSharp.HSM.Tests  

open FSharp.HSM
open System
open NUnit.Framework
open FsUnit
open TestHelpers

module OrderTest =
    
    type State = 
     | Created
     | Sent
     | Working
     | PartFilled
     | Completed
     | Filled
     | Rejected
     | Canceled

    type Fill = { Price: float; Quantity: int; Time: int64; }
    type Trigger =
     | Send 
     | ExchAck
     | Fill// of Fill
     | Cancel
     | ExchReject
     //| Amend
    
    exception Overfill

    

    type Order(qty) = 
        let mutable qtyFilled = 0
        let fillFunc (fill: Fill option) = 
            match fill with
            | Some f -> qtyFilled <- qtyFilled + f.Quantity
            | None -> ()
            if qtyFilled = qty then 
                Some(Filled) 
            elif qtyFilled > qty then 
                raise Overfill
            elif qtyFilled > 0 then
                Some(PartFilled)
            else Some(Working)//should actually return current state...
        //handle fill, if fully filled return Filled else PartFilled
        let state = 
          create
            [ configure Created
                |> on Send Sent //handle
              configure Sent
                |> on ExchAck Working
              configure Working
                |> handle Trigger.Fill fillFunc //handle 
                |> on ExchReject Rejected
              configure PartFilled
                |> substateOf Working 
              configure Completed
              configure Filled 
                |> substateOf Completed 
              configure Rejected 
                |> substateOf Completed
              configure Canceled 
                |> substateOf Completed ] 
        member this.State with get() = state

    let check order trueStates falseStates  = 
        trueStates |> List.iter (isInState order)
        falseStates |> List.iter (isNotInState order)

    [<Test>]
    let ``Partfill``() =
      let order =(new Order(4)).State
      attachShow order

      order.Init Created

      fire order Send
      check order [Sent] [Working; PartFilled; Completed; Filled; Rejected]

      fire order ExchAck
      check order [Working] [Sent; PartFilled; Completed; Filled; Rejected]

      fireW order Fill  { Price = 3.0; Quantity = 2; Time = 3L; }
      check order [Working; PartFilled] [Sent; Completed; Filled; Rejected]

      fireW order Fill  { Price = 3.0; Quantity = 2; Time = 3L; }
      check order [Filled; Completed] [Sent; Working; PartFilled; Canceled; Rejected]

