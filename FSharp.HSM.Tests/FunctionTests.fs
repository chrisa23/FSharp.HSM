module FunctionTests

open FSharp.HSM
open System
open NUnit.Framework
open FsUnit

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
        configure State2
        configure State3
        configure State4
    ]

[<TestFixture>]
type Tests() =
    [<Test>]
    let ``find can find a state`` =
        let found = find testStateList1 State1

        found.State |> should equal State1