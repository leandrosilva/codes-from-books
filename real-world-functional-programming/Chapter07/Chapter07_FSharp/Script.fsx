// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 7
// --------------------------------------------------------------------------
// This file contains code samples that are not related to the primary
// example of the chapter (document drawing application) and are intended
// for separate evaluation using the F# interactive
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 7.1 Functional data structures

// Listing 7.1 Representing rectangle using record type 

// Declaration of 'Rect' record
type Rect =   
  { Left : float32
    Top : float32
    Width : float32
    Height : float32 }

// Creating a record value
let rc = 
  { Left = 10.0f; Top = 10.0f;
    Width = 100.0f; Height = 200.0f; }
           
// Accessing elements using the name
rc.Left + rc.Width

// --------------------------------------------------------------------------
// Section 7.1.1 Using F# record type

// Listing 7.2 Functions for working with rectangles
open System.Drawing

let deflate(rc, wspace, hspace) = 
  // Create and return deflated rectangle
  { Top = rc.Top + wspace
    Left = rc.Left + hspace
    Width = rc.Width - (2.0f * wspace)
    Height = rc.Height - (2.0f * hspace) }
     
let toRectangleF rc = 
  // Return a new instance of 'RectangleF' class
  RectangleF(rc.Left, rc.Top, rc.Width, rc.Height)

let it = 
  { Left = 0.0f; Top = 0.0f;
    Width = 100.0f; Height = 100.0f; };;

// Test 'deflate' using rectangle from the previous command
deflate(it, 20.0f, 10.0f)
