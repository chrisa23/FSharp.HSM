namespace PelicanDemo

open System
open System.Windows
open System.Windows.Threading
open System.Threading
open System.Windows.Media
open System.Windows.Media.Imaging
open System.IO
open FSharp.HSM.Tests
open PELICAN

type Sign() as grid =
    inherit Controls.Grid(Margin=Thickness 1.0)
    let hsm = PELICAN.PelicanSignal()
    
    let green = BitmapImage(Uri("Green.jpg", UriKind.Relative))
    let yellow = BitmapImage(Uri("Yellow.jpg", UriKind.Relative))
    let red = BitmapImage(Uri("Red.jpg", UriKind.Relative))
    let walkI = BitmapImage(Uri("Walk.jpg", UriKind.Relative))
    let dontWalk = BitmapImage(Uri("DontWalk.jpg", UriKind.Relative))

    do

        let row = Controls.RowDefinition()
        row.Height <- GridLength 225.
        grid.RowDefinitions.Add row

        let row = Controls.RowDefinition()
        row.Height <- GridLength 160.
        grid.RowDefinitions.Add row

        let row = Controls.RowDefinition()
        row.Height <- GridLength 160.
        grid.RowDefinitions.Add row

        let row = Controls.RowDefinition()
        row.Height <- GridLength 50.
        grid.RowDefinitions.Add row

        let state = Controls.TextBlock()
        state.HorizontalAlignment <- HorizontalAlignment.Center
        Controls.Grid.SetRow(state,3)
        let walk = Controls.Image()
        walk.HorizontalAlignment <- HorizontalAlignment.Center
        let car = Controls.Image()
        car.HorizontalAlignment <- HorizontalAlignment.Center
        let bn = Controls.Button()
        bn.Content <- "WALK"
        bn.Margin <- Thickness 5.0
        bn.Height <- 130.
        bn.Width <- 130.
        bn.FontSize <- 28.
        bn.HorizontalAlignment <- HorizontalAlignment.Center
        Controls.Grid.SetRow(bn,2)
        Controls.Grid.SetRow(walk,1)
        Controls.Grid.SetRow(car,0)
        grid.Children.Add bn |> ignore
        grid.Children.Add walk |> ignore
        grid.Children.Add car |> ignore
        grid.Children.Add state |> ignore
        
        bn.Click.Add(fun _ -> 
            try
                hsm.Walk()
            with 
            _ -> ())
  
        hsm.VehicleSignal.Add(fun x -> car.Dispatcher.Invoke(fun () -> 
            match x with
            | Green -> car.Source <- green
            | Yellow -> car.Source <- yellow
            | Red -> car.Source <- red))
        hsm.PedestrianSignal.Add(fun x -> walk.Dispatcher.Invoke(fun () -> 
            match x with
            | DontWalk -> walk.Source <- dontWalk
            | Blank ->  walk.Source <- null
            | Walk -> walk.Source <- walkI))
        hsm.State.Add(fun x -> walk.Dispatcher.Invoke(fun () -> 
            state.Text <- sprintf "%A" x))
        hsm.Start()

module Program =         
         
    [<System.STAThread>]
        Window(Title="Pelican",
            Content=Controls.Viewbox(Child=Sign()),
            Width=200.,
            Height=700.)
        |> Application().Run
        |> ignore
