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
let w = loadXamlWindow(@"Window2.xaml") // Fill the appropriate path here
w.Show()

[<STAThread>]
do app.Run(w) |>ignore
