// Expert F# 2.0
// Chapter 11

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media

let w = new Window(Topmost=true)
w.Show()
let c = new Canvas()
w.Content <- c

let r = new Rectangle(Width=100.,Height=100.,RadiusX=5.,RadiusY=5.,Stroke=Brushes.Black)
c.Children.Add(r)

let e = new Ellipse(Width=150.,Height=150.,Stroke=Brushes.Black)
c.Children.Add(e)
Canvas.SetLeft(e, 100.)
Canvas.SetTop(e, 100.)

e.MouseLeftButtonUp.Add(fun _ ->
  e.Fill <- 
    if e.Fill = (Brushes.Yellow :> Brush) then Brushes.Red
    else Brushes.Yellow
)

