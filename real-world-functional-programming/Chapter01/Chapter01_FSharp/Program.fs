#light
// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 1
// --------------------------------------------------------------------------
// This file is a main file of the project. The code in this file can be 
// executed by running the project. Other F# examples are provided
// in the script file 'Script.fsx'.
// --------------------------------------------------------------------------

// Listing 1.12 Object-oriented Hello world using Windows Forms (F#)   

// Import necessary .NET namespaces
open System.Drawing
open System.Windows.Forms

// F# class declaration
type HelloWindow() =
   // Constructor initializes the user interface
   let frm = new Form(Width = 400, Height = 140)
   let fnt = new Font("Times New Roman", 28.0f)
   let lbl = new Label(Dock = DockStyle.Fill, Font = fnt,
                       TextAlign = ContentAlignment.MiddleCenter)
   do frm.Controls.Add(lbl)

   // Builds and displays the hello message
   member x.SayHello(name) =
      let msg = "Hello " + name + "!"
      // Modify property of a .NET type
      lbl.Text <- msg

   // Method that runs the application
   member x.Run() =      
      Application.Run(frm)
      
// Create an instance of the window and run it      
let hello = new HelloWindow()
hello.SayHello("dear reader")
hello.Run()


