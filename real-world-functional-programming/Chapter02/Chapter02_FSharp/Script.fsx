// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 2
// --------------------------------------------------------------------------
// This is an F# script file - to run a the code, select it and
// hit Alt+Enter. The code will be executed using F# interactive
// --------------------------------------------------------------------------


// --------------------------------------------------------------------------
// Section 2.3.2 Higher order functions

// Listing 2.3 Working with lists using higher order functions (F# interactive)

let numbers = [ 1 .. 10 ]
// Is the number 'n' odd?
let isOdd(n) = (n%2 = 1)
// Returns square of a number
let square(n) = n * n

// Filter numbers using 'isOdd' function
List.filter isOdd numbers

// Filter and apply 'square' to every number
List.map square (List.filter isOdd numbers)


// Listing 2.4 Elegant way for working with functions (F# interactive)
let squared = 
   numbers                  // Take the list of numbers
   |> List.filter isOdd     // Select odd numbers
   |> List.map square       // Calculate square of each number

// --------------------------------------------------------------------------
// Section 2.4.1 Type inference in C# and F#

open System

// Listing 2.5 Implementing methods with type inference
let add a b =
   // Add two numbers
   let res = a + b
   // Format the returned string
   String.Format("{0} + {1} = {2}", a, b, res)

// --------------------------------------------------------------------------
// Section 2.4.2 Introducing the discriminated union type
open System.Drawing

type Shape = 
   // Rectangle with left-upper and right-lower point
   | Rectangle of Point * Point
   // Ellipse with the bounding rectangle 
   | Ellipse of Point * Point
   // A shape composed from two shapes
   | Composed of Shape * Shape
   

// Utility functions that are needed for the following code to compile
// (and don't do any real work)
let rectangleArea(pfrom, pto) = 0
let ellipseArea(pfrom, pto) = 0
let isNestedRectangle(pf1, pt1, pf2, pt2) = false
let intersectionArea(shape1, shape2) = 0

// Section 2.4.3 Pattern matching

// Listing 2.7 Calculating area using pattern matching (F#)
let rec shapeArea(shape) = 
   match shape with
   | Rectangle(pfrom, pto) ->
       // Calculate area of a rectangle
       rectangleArea(pfrom, pto)       
   | Ellipse(pfrom, pto) ->
        ellipseArea(pfrom, pto)        
   // Case for a rectangle nested inside another
   | Composed(Rectangle(from1, to1), Rectangle(from2, to2))
           when isNestedRectangle(from2, to2, from1, to1) ->
      // Optimized version
      rectangleArea(from1, to1)      
   // Remaining case
   | Composed(shape1, shape2) ->
      // Calculate area of composed shape
      let area1 = shapeArea(shape1)
      let area2 = shapeArea(shape2)
      area1 + area2 - (intersectionArea(shape1, shape2))
  

// --------------------------------------------------------------------------
// Section 2.4.4 Compile-time program checking

// Required declarations of units of measure types
// (this is fully explained later in chapter 13)
[<Measure>] type mile
[<Measure>] type km
[<Measure>] type h

// Listing 2.8 Calculating with speed using units of measure (F# interactive)

// Maximal allowed speed in km/h
let maxSpeed = 50.0<km/h>
// Actual speed in mph
let actualSpeed = 40.0<mile/h>

// Is the speed larger?
(*
if (actualSpeed > maxSpeed) then // Error: The types are not compatible
    printfn "Speeding!"
*)

// Implement conversion of units
let mphToKmph(speed:float<mile/h>) =
   speed * 1.6<km/mile>

// Correct comparison using conversion
if (mphToKmph(actualSpeed) > maxSpeed) then
   printfn "Speeding!"
