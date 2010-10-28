// Expert F# 2.0
// Chapter 11

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

open System.Windows
open System.Windows.Controls

let w = new Window()

let b = new Button(Content="Hello from WPF!")
b.Click.Add(fun _ -> w.Close())

w.Content <- b
w.Show()
