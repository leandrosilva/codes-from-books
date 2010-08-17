// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 9
// --------------------------------------------------------------------------
// Note: This file contains most F# source code for the C# <-> F#
// interoperability examples. This should be compiled as a 'Class Library'
// (see properties of the project).
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 9.5 Using F# libraries from C#

// Listing 9.19 Compiling F# types into a library

// Specifies namespace for the file
namespace Chapter09.FSharpExport

open System
open System.Drawing

type Rect =
  // Record fields are compiled as properties
  { Left : float32; Width : float32
    Top : float32; Height : float32 }
  
  // Methods of the 'Rect' class
  member x.Deflate(wspace, hspace) =
    { Top = x.Top + wspace; Height = x.Height - (2.0f * hspace)
      Left = x.Left + hspace; Width = x.Width - (2.0f * wspace) }
  member x.ToRectangleF () = 
    RectangleF(x.Left, x.Top, x.Width, x.Height)


// --------------------------------------------------------------------------
// Section 9.5.1 Working with values and delegates 

// Listing 9.21  Exporting values and higher order functions

// Enclose values and functions in a module
type Client =
  { Name : string; Income : int; YearsInJob : int
    UsesCreditCard : bool; CriminalRecord : bool }
  
module Tests =
  let John = 
    { Name = "John Doe"; Income = 25000; YearsInJob = 1
      UsesCreditCard = true; CriminalRecord = false }

  // Function taking delegate as an argument
  let WithIncome (f:Func<_, _>) client =                   
    // Calls the delegate using 'Invoke' method
    { client with Income = f.Invoke(client.Income) }