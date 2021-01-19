namespace FSharp.HSM.Tests

open FSharp.HSM
open System
open NUnit.Framework
open FsUnit
open TestHelpers

module FunctionTests =

    type States =
        | State1
        | State2
        | State3
        | State4

    type Events =
        | Event1
        | Event2
        | Event3
        | Event4

    type Output =
        | Output1
        | Output2

    let testStateList1: StateConfig<States, Events, Output, unit> list =
        [ configure State1
          |> onEntry (fun () -> printfn "Enter State1")
          |> on Event1 State2
          |> on Event2 State1
          configure State2
          |> substateOf State1
          |> on Event3 State3
          configure State3
          |> substateOf State2
          |> on Event3 State4
          configure State4
          |> substateOf State1
          |> on Event4 State3
          |> enterRaises Output2
          |> exitRaises Output1 ]

    //[<Test>]
    //let ``find can find a state``() =
    //    let found = find testStateList1 State1

    //    found.State |> should equal State1


    [<Test>]
    let ``history test`` () =
        let hsm = create testStateList1
        let store = ResizeArray<States>()

        attachHistory hsm store

        hsm.Init State1

        hsm.Fire Event1

        hsm.Fire Event3
        hsm.Fire Event3

        hsm.Fire Event2

        let history = store |> Seq.toList

        history
        |> should
            equal
            [ State1
              State2
              State3
              State4
              State1 ]


    [<Test>]
    let ``permitted test`` () =
        let hsm = create testStateList1

        hsm.Init State1

        hsm.Fire Event1

        hsm.Fire Event3
        hsm.Fire Event3

        hsm.Permitted
        |> should equal [| Event1; Event2; Event4 |]


    [<Test>]
    let ``output test`` () =
        let hsm = create testStateList1
        let store = ResizeArray<Output>()
        attachOutputHistory hsm store

        hsm.Init State1

        hsm.Fire Event1

        hsm.Fire Event3
        hsm.Fire Event3

        let history = store |> Seq.toList
        history |> should equal [ Output2 ]

        hsm.Fire Event1

        let history = store |> Seq.toList
        history |> should equal [ Output2; Output1 ]
