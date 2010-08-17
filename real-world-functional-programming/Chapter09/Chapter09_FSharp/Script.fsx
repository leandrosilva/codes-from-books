// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 9
// --------------------------------------------------------------------------
// Note: This file contains most of the source code with the
// exception of the 'Using F# libraries from C#' samples that can
// be found in 'Export.fs'
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 9.1 Improving data-centric applications

open System.Drawing

// Listing 9.1 'Rect' type with processing functions
module Listing_9_1 = 
  // Type declaration
  type Rect =
    { Left   : float32
      Top    : float32
      Width  : float32
      Height : float32 }                       

  // Shrinks the rectangle
  let deflate(rc, wspace, hspace) = 
    { Top = rc.Top + wspace
      Left = rc.Left + hspace
      Width = rc.Width - (2.0f * wspace)
      Height = rc.Height - (2.0f * hspace) }
  
  // Conversion to 'System.Drawing' representation
  let toRectangleF(rc) = 
    RectangleF(rc.Left, rc.Top, rc.Width, rc.Height);;

// --------------------------------------------------------------------------
// Section 9.1.1 Adding members to F# types

// Listing 9.2 'Rect' type with operations as members

// Usual declaration of F# record type 
type Rect =
    { Left   : float32
      Top    : float32
      Width  : float32
      Height : float32 }
    /// Creates a rectangle which is deflated by 'wspace' from the 
    /// left and right and by 'hspace' from the top and bottom
    member x.Deflate(wspace, hspace) =
      { Top = x.Top + wspace
        Left = x.Left + hspace
        Width = x.Width - (2.0f * wspace)
        Height = x.Height - (2.0f * hspace) }
    /// Converts the rectangle to representation from 'System.Drawing'
    member x.ToRectangleF () = 
      RectangleF(x.Left, x.Top, x.Width, x.Height)

// Listing 9.3 Working with types and members   
// Create a 'Rect' value 
let rc = 
 { Left = 0.0f; Top = 0.0f
   Width = 100.0f; Height = 100.0f }

// The first member returns deflated rectangle
rc.Deflate(10.0f, 30.0f)
// The second member returns 'RectangleF' value
rc.ToRectangleF()


// --------------------------------------------------------------------------
// Section 9.1.2 Appending members using augmentations

open System

// Listing 9.4 Schedule data type with a function 

// Original type declaration
type Schedule =
  | Never
  | Once of DateTime
  | Repeatedly of DateTime * TimeSpan

// Calculate the next occurence of an event
let futureOrMaxValue(dt) = 
  if (dt > DateTime.Now) then dt else DateTime.MaxValue

let getNextOccurrence(schedule) =
  match schedule with
  | Never -> DateTime.MaxValue
  | Once(eventDate) -> futureOrMaxValue(eventDate)
  | Repeatedly(startDate, interval) ->
      // How many times will it occur until today?
      let q = (DateTime.Now - startDate).TotalSeconds / interval.TotalSeconds
      // Only consider occurrences after start date
      let q = max q 0.0
      // Calculate first occurrence after today
      startDate.AddSeconds(interval.TotalSeconds * (Math.Floor(q) + 1.0))

// Listing 9.5 Adding members using type extensions

// Type extension
type Schedule with
  // Member just calls the function
  member x.GetNextOccurrence() = getNextOccurrence(x)
  member x.OccursNextWeek() = 
    getNextOccurrence(x) < DateTime.Now.AddDays(7.0) 

// Listing 9.6 Working with schedule using members
let sch1 = Repeatedly(DateTime(2000, 7, 24), TimeSpan(365, 0, 0, 0))
sch1.OccursNextWeek()

let sch2 = Never
sch2.OccursNextWeek()

// --------------------------------------------------------------------------
// Section 9.2 Improving behavior-centric applications 

// Type declaration and value from the Chapter 7
type Client = {
  Name : string; Income : int; YearsInJob : int
  UsesCreditCard : bool; CriminalRecord : bool }

let john = 
  { Name = "John Doe"; Income = 40000; YearsInJob = 1
    UsesCreditCard = true; CriminalRecord = false }

// Listing 9.7 Testing clients using records of functions
module Listing_9_7 =
  // Representation of the test
  type ClientTests = 
    { Test : Client -> bool
      Report : Client -> unit }
     
  // Testing and reporting function
  let testCriminal(cl) = cl.CriminalRecord = true                     
  let reportCriminal(cl) =                                            
    printfn "'%s' has a criminal record!" cl.Name                  
  let testIncome(cl) = cl.Income < 30000                              
  let reportIncome(cl) =                                              
    printfn "Income of '%s' is less than 30000!" cl.Name           

  // Create record values
  let tests =                                                        
    [ { Test = testCriminal; Report = reportCriminal };
      { Test = testIncome;   Report = reportIncome };
    (* more tests... *) ]
    

// --------------------------------------------------------------------------
// 9.2.1 Using interface object types 

// Listing 9.8 Interface representing client test

// Interface type declaration
type ClientTests = 
  // Member that tests the client
  abstract Test : Client -> bool
  // Member that reports issues
  abstract Report : Client -> unit 


// Listing 9.9 Implementing interfaces using object expressions

// Object expression
let testCriminal =
  // Provides code for the interface members
  { new ClientTests with
      member x.Test cl = cl.CriminalRecord = true
      member x.Report cl =
        printfn "'%s' has a criminal record!" cl.Name }

let testIncome =
  { new ClientTests with
      // Implements testing of a client
      member x.Test cl = cl.Income < 30000
      // Implements reporting
      member x.Report cl =
        printfn "Income of '%s' is less than 30000!" cl.Name }
          
// Create a list of interface values
let tests = [ testCriminal; testIncome ]
  

// --------------------------------------------------------------------------
// Section 9.3 Working with .NET interfaces

// Listing 9.10 Implementing 'IEqualityComparer<'T>' interface

open System
open System.Collections.Generic

// Create object implementing 'IEqualityComparer<string>'
let equality = 
  // Utility function that removes spaces from the string
  let replace(s:string) = s.Replace(" ", "")
  { new IEqualityComparer<_> with
      // Compare strings, ignoring the spaces
      member x.Equals(a, b) = 
        String.Equals(replace(a), replace(b))
      // Get hash code of the string without spaces
      member x.GetHashCode(s) = 
        replace(s).GetHashCode() }
      
// Mutate the collection
let dict = new Dictionary<_, _>(equality)
dict.Add("100", "Hundred")
dict.Add("1 000", "thousand")
dict.Add("1 000 000", "million")

// Get value using the custom comparer
dict.["100"]
dict.["10 00"]
dict.["1000000"]


// --------------------------------------------------------------------------
// Section 9.3.2 Cleaning resources using IDisposable

// Listing 9.11 Working with files and the 'use' keyword

open System
open System.IO

let readFile() =
  // Declare the 'sw' value using the 'use' keyword
  use reader = new StreamReader("C:\\temp\\test.txt")
  let text = reader.ReadToEnd()
  Console.Write(text)
  // Dispose called after this line

// Write hte file to the disk
readFile()

// Specifying scope explicitly
let test() = 
   let text = 
      use reader = new StreamReader("C:\\temp\\test.txt")
      reader.ReadToEnd()
   Console.Write(text)


// Listing 9.13 Setting console color using IDisposable
open System

let changeColor clr = 
  // Store the original color
  let orig = Console.ForegroundColor
  // Set the new color immediately
  Console.ForegroundColor <- clr
  // Create 'IDisposable' value
  { new IDisposable with
      member x.Dispose() = 
        // Restore the original color inside 'Dispose' 
        Console.ForegroundColor <- orig }

let hello () = 
  // Color is changed to red
  use n = changeColor ConsoleColor.Red
  Console.WriteLine("Hello world!")
  // Original color is restored
  
Console.WriteLine("Calling hello...")
hello ()
Console.WriteLine(".. done!")
  

// --------------------------------------------------------------------------
// Section 9.4 Concrete object types

// Listing 9.14 Class with client information (F# interactive)
module Listing_9_14 =

  // Class declaration with constructor arguments
  type ClientInfo(name, income, years) =
    // Code executed during construction
    let q = income / 5000 * years
    do printfn "Creating client '%s'" name

    // Member declarations
    member x.Name = name
    member x.Income = income
    member x.Years = years

    // Method declaration
    member x.Report() =
      printfn "Client: %s, q=%d" name q

  // Create the class and run the constructor
  let john = new ClientInfo("John Doe", 40000, 2)
  // Invoke method of the class
  john.Report()

// --------------------------------------------------------------------------
// Section 9.4.1 Functional and imperative classes

// Listing 9.15 Imperative and Functional and imperative version of Client type

// Functional 'Client' class
type ClientF(name, income) =
  // Memebers are immutable
  member x.Name = name
  member x.Income = income
  
  // Returns a new instance 
  member x.WithIncome(ninc) =
    new ClientF(name, ninc)
  member x.Report() =
    printfn "%s %d" name income

// Imperative 'Client' class
type ClientI(name, income) =
  // Private mutable state
  let mutable income = income
  member x.Name = name
  // Get & set property exposing the state
  member x.Income 
    with get() = income
    and set(v) = income <- v
  member x.Report() =
    printfn "%s %d" name income

// --------------------------------------------------------------------------
// Section 9.4.2 Implementing interfaces and casting 

type ClientTest = 
  abstract Test : Client -> bool
  abstract Report : Client -> unit

// Listing 9.17 Implementing interface in a class

// Implicit class declaration
type CoefficientTest(income, years, min) =

  // Local helper functions
  let coeff(client) =
    float(client.Income)*income + float(client.YearsInJob)*years
  let report(client) =
    printfn "Coefficient %f is less than %f." (coeff(client)) min

  // Standard public method of the class
  member x.PrintInfo() =
    printfn "income*%f + years*%f > %f" income years min

  // Interface implementation using helpers
  interface ClientTest with
    member x.Report(client) = report(client)
    member x.Test(client) = coeff(client) < min


// Listing 9.18 Working with F# classes and interfaces
do
  // Create an instance of the class
  let test = new CoefficientTest(0.001, 5.0, 50.0)
  // Use method of the class
  test.PrintInfo()

  // Cast to the interface type
  let cltest = (test :> ClientTest)
  // Use methods of the interface
  if (cltest.Test(john)) then 
    cltest.Report(john)
