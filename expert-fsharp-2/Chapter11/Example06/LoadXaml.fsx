// Expert F# 2.0
// Chapter 11

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Shapes
open System.Xml

let loadXamlWindow (filename:string) =
  let reader = XmlReader.Create(filename)
  XamlReader.Load(reader) :?> Window

let app = new Application()
let w = loadXamlWindow(@"Window1.xaml") // Fill the appropriate path here
w.Show()

let e = w.FindName("Circle") :?> Ellipse

e.MouseLeftButtonUp.Add(fun _ ->
  e.Fill <- 
    if e.Fill = (Brushes.Yellow :> Brush) then Brushes.Red
    else Brushes.Yellow
)

[<STAThread>]
do app.Run(w) |>ignore
