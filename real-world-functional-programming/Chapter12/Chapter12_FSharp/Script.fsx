﻿// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 12
// --------------------------------------------------------------------------


// --------------------------------------------------------------------------
// Section 12.1 Generating sequences

// Listing 12.2 Introducing sequence expression syntax

// Expression wrapped inside 'seq' 
let nums = seq {
  let n = 10 
  // Return element of the sequence
  yield n + 1
  // Side-effect inside sequence expression
  printfn "second.."
  yield n + 2 }

// Value is a sequence of numbers
nums

// Listing 12.3 Composing sequences from different sources

// List of capital cities
let capitals = [ "London"; "Prague" ]
// Return a name and a name with a prefix
let withNew(x) =  
   seq { yield x
         yield "New " + x }

let allCities =          
  seq { // Return a single value
        yield "Seattle"
        // Compose with another sequence
        yield! capitals
        // Return all cities generated by the function
        yield! withNew("York") }

// All data composed together
allCities |> List.ofSeq

// ----------------------------------------------------------------------------
// Section 12.2 Mastering sequence expressions   

// Listing 12.4 Generating factorials using sequence expressions
open System

// Recursive utility function
let rec factorialsUtil(num, factorial) = seq {
   if (factorial < 1000000) then
      // Return single result
      yield String.Format("{0}! = {1}", num, factorial)
   if (factorial < 1000000) then // We can delete this line in the next release of F#
      let num = num + 1
      // Recursively generate remaining factorials
      yield! factorialsUtil(num, factorial * num) }
      
let factorials = 
  // Get sequence starting from the first factorial
  factorialsUtil(0, 1)

// ----------------------------------------------------------------------------
// Section 12.2.2 Using infinite sequences 

open System
open System.Drawing
open System.Windows.Forms

// Listing 12.5 Generating infinite sequence of random colors

let rnd = new Random()
let rec colorsRnd = 
  seq { let c() = rnd.Next(256) 
        yield Color.FromArgb
                 (c(), c(), c())
        yield! colorsRnd  }

// Listing 12.7 Generating a sequence with color gradients

let rec colorsGreenBlack = seq {
   for i in 0 .. 25 .. 255 do
      yield Color.FromArgb
       (i / 2, i, i / 3)
   yield! colorsGreenBlack }

// Listing 12.6 Drawing a chart using sequence of colors

let numbers = [ 490; 485; 450; 425; 365; 340; 290; 230; 130; 90; 70; ]

// Combine data with colors into one sequence
let clrData1 = Seq.zip numbers colorsGreenBlack |> List.of_seq
let clrData = Seq.zip numbers colorsRnd |> List.of_seq

let frm = new Form(ClientSize = Size(500, 350))
frm.Paint.Add(fun e -> 
   e.Graphics.FillRectangle(Brushes.White, 0, 0, 500, 350)
   // Iterate over data and colors with index
   clrData |> Seq.iteri(fun i (num, clr) ->
      use br = new SolidBrush(clr)
      // Calculate location of the bar using index
      e.Graphics.FillRectangle(br, 0, i * 32, num, 28) )
   )
frm.Show()   

// ----------------------------------------------------------------------------
// Section 12.3.3 Flattening projections

let cities = [ ("New York", "USA"); ("London", "UK");
               ("Cambridge", "UK"); ("Cambridge", "USA") ]
let entered = [ "London"; "Cambridge" ]


// Listing 12.10 Joining collections using sequence expressions

seq { 
  // Return a collection for each entered city
  for name in entered do
    // Iterate over all known cities 
    for (n, c) in cities do
      // Yield zero or one element for each combination
      if (n = name) then
        yield sprintf "%s from %s" n c }


// Listing 12.11 Replacing outer loop with flattening projection

// Replace loop with flattening projection
entered |> Seq.collect (fun name ->
  // For each city, return a sequence
  seq { for (n, c) in cities do
          if (n = name) then
            yield sprintf "%s from %s" n c })


// Listing 12.12 Replacing both loops with flattening projection

entered |> Seq.collect (fun name ->
  // Find all cities and format the output
  cities |> Seq.collect (fun (n, c) -> 
    // Return zero or one items
    if (n <> name) then []
    else [ sprintf "%s from %s" n c ]))


// ----------------------------------------------------------------------------
// Section 12.5 First steps in custom computations

open System

// Listing 12.17 Value of the computation
type ValueWrapper<'a> =
  | Value of 'a

// Listing 12.19 Implementing computation builder for values
type ValueWrapperBuilder() = 
  member x.Bind(Value(v), f) = f(v)
  member x.Return(v) = Value(v)
  
let value = new ValueWrapperBuilder()

// Function that returns number fromt he console as a wrapped value
let readInt() =  value { 
  let n = Int32.Parse(Console.ReadLine())
  return n }

// Listing 12.18 Calculating with computation values
let v = 
  value { 
    let! n = readInt()
    let! m = readInt()
    let add = n + m
    let sub = n - m
    return add * sub }


// ----------------------------------------------------------------------------
// Section 12.6 Implementing computation expressions for options

// Listing 12.21 Computation builder for option type

type OptionBuilder() = 
  member x.Bind(v, f) = 
    // Unwrap the option value
    match v with 
    | Some(value) -> 
        // Run the rest of the computation
        f(value)
    | _ -> 
        // The result is undefined
        None
  member x.Return(v) = 
    // Wrap actual value
    Some(v)
  
let option = new OptionBuilder()

// Demo: Working with option values  
let tryReadInt() = 
  let succ, n = Int32.TryParse(Console.ReadLine())
  if (succ) then Some(n) else None

let testIt() =  
  option {
     let! n = tryReadInt()
     let! m = tryReadInt()
     return n * m 
  }
    

// ----------------------------------------------------------------------------
// Section 12.7 Augmenting computations with logging

module Logging = 
  open System
  
  type LoggingValue<'a> = 
    | Log of 'a * list<string>

  // Listing 12.23 Computation builder that adds logging support 
               
  type LoggingBuilder() =
    member x.Bind(Log(v, logs1), f) =  // Unwrap the value and log buffer
      // Run the rest of the computation
      let (Log(nv, logs2)) = f(v)
      // Wrap the value and merge log buffers
      Log(nv, logs1 @ logs2)
    
    // Augment value with an empty log
    member x.Return(v) = Log(v, [])
    // No value with an empty log
    member x.Zero() = Log((), [])
    
  let log = new LoggingBuilder()
  let logMessage(s) = Log((), [s])

  // Listing 12.24 Logging using computation expressions    
  
  // Writes string to console and to the log
  let write(s) = log { 
    do! logMessage("writing: " + s)
    Console.Write(s) }

  let read() = log { 
    do! logMessage("reading...")
    return Console.ReadLine() }
      
  let testIt () = log { 
    // Call the primitive logging function
    do! logMessage("starting...")
    // Call function written using computation expressions
    do! write("Enter your name:")
    // Using customized value binding
    let! name = read()
    return "Hello " + name + "!" }



  // ----------------------------------------------------------------------------
  // Shows how the code is translated to member calls
  module Translations =
    
    let write(s) =
      log.Bind(logMessage("writing: " + s), fun () ->
         Console.Write(s)
         log.Zero())

    let read() = 
      log.Bind(logMessage("reading"), fun () ->
        log.Return(Console.ReadLine()))

    let testIt() = 
      log.Bind(logMessage("starting"), fun () ->
        log.Bind(write("Enter name: "), fun () ->
          log.Bind(read(), fun name ->
            log.Return("Hello " + name + "!"))))


// ----------------------------------------------------------------------------
// BONUS: doing the same thing using the 'yield' keyword

module LoggingUsingYield = 
  type LoggingValue<'a> = 
    | Log of 'a * list<string>
             
  let bind (Log(v, logs1)) f =            
    let (Log(nv, logs2)) = f(v)
    Log(nv, logs1 @ logs2)
    
  type LoggingBuilder() =
    member x.Bind(v, f) = bind v f
    member x.Return(v) = Log(v, [])
    // Creates monadic value representing 'non-standard' unit value
    member x.Zero() = Log((), [])
    // Support for the 'yield' keyword that allows us to 
    // generate log values that carry only 'logging message'
    member x.Yield(s) = Log((), [s])
    // Combines two logging values and assumes that the first one
    // returns 'unit' (e.g. value created by Yield or Zero)
    member x.Combine(a, b) = bind a (fun () -> b)
    member x.Delay(f) = f()
    
  let log = new LoggingBuilder()

  open System
    
  let write(s) = log { 
    yield "writing: " + s
    Console.Write(s) }

  let read() = log { 
    yield "reading..."
    return Console.ReadLine() }
      
  let testIt () = log { 
     yield "starting..."
     do! write("Enter your name:")
     let! name = read()
     return "Hello " + name + "!" }