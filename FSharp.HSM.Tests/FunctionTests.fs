namespace FSharp.HSM.Tests  

open FSharp.HSM
open System
open NUnit.Framework
open FsUnit


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

    let testStateList1 :StateConfig<States,Events> list = 
        [ 
            configure State1 
                |> onEntry (fun () -> printfn "Enter State1")
            configure State2
                |> substateOf State1
            configure State3
                |> substateOf State2
                |> on Event3 State4
            configure State4
                |> substateOf State1
                |> on Event4 State3
        ]

    [<Test>]
    let ``find can find a state``() =
        let found = find testStateList1 State1

        found.State |> should equal State1

    
    [<Test>]
    let ``parent enter test``() = ()
        