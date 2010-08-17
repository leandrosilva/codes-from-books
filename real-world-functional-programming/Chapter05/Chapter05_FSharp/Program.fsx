// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 5
// --------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Section 5.1: Multiple value

// Listing 5.1: Division with a remainder
let divRem(a, b) =
    // Return a tuple
    (a / b, a % b)

// Calling the function
let (res, rem) = divRem(10, 3)  

// Using tuples instead of 'out' parameter
open System
let (succ, num) = Int32.TryParse("41")

// Listing 5.2 Different representations of a message with coordinates
// This is just a stub, that we need in the following code sample
let printMessage (x:int, y:int) (s:string) = ()

let msgAt1 = (50, 100, "Hello!")
let msgAt2 = ((50, 100), "Hello!")

// We have to extract all element
let (x, y, _) = msgAt1
printMessage (x, y) "Test!"

// We need to extract only the first element
let (coords, _) = msgAt2
printMessage coords "Test!"

// The simplest way using the second representation
printMessage (fst(msgAt2)) "Message Test!"


// ----------------------------------------------------------------------------
// Section 5.2: Alternative value

// Listing 5.3 Schedule type using discriminated union
type Schedule =
    // Unscheduled event with no argument
    | Never
    // Event with single occurrence
    | Once of DateTime
    // Repeated event with first occurrence and periodicity
    | Repeatedly of DateTime * TimeSpan


// Listing 5.4 Creating values of discriminated union

// Create values representing times and periodicity
let tomorrow = DateTime.Now.AddDays(1.0)
let noon = new DateTime(2008, 8, 1, 12, 0, 0)
let daySpan = new TimeSpan(24, 0, 0)

// Create schedule using discriminator 'Never'
let schedule1 = Never
// Event occurring once at specified time
let schedule2 = Once(tomorrow)
// Event occurring repeatedly every day
let schedule3 = Repeatedly(noon, daySpan)


// Listing 5.5 Calculate the next occurence of an event

let getNextOccurrence(schedule) =
  match schedule with
  | Never -> DateTime.MinValue
  | Once(dt) -> 
      if (dt > DateTime.Now) then dt 
      else DateTime.MinValue
  | Repeatedly(startDate, interval) ->
      // How many times will it occur until today?
      let q = (DateTime.Now - startDate).TotalSeconds / interval.TotalSeconds
      // Only consider occurrences after start date
      let q = max q 0.0
      // Calculate first occurrence after today
      startDate.AddSeconds(interval.TotalSeconds * (Math.Floor(q) + 1.0))


// ----------------------------------------------------------------------------
// Section 5.2.3: Using option type in F#

// Listing 5.7 Reading input as option value

let readInput() =
    let s = Console.ReadLine()
    // Try to parse the input
    let (succ, num) = Int32.TryParse(s)
    if (succ) then
        // Return value using 'Some' discriminator
        Some(num)
    else
        // Return undefined value using 'None'
        None;;

// Listing 5.8 Processing input using option value

let testInput() =
    let inp = readInput()
    // We cannot use the value directly!
    // Check for alternatives using pattern matching
    match inp with
    | Some(v) ->
        // Branch for correct input
        printfn "You entered: %d" v
    | None ->
        // Branch for undefined input
        printfn "Incorrect input!";;

// Test the function interactively
testInput()


// ----------------------------------------------------------------------------
// Section: 5.3.4 Writing generic functions

let readValue(opt) =
    match opt with
    | Some(v) -> v
    | None -> failwith "No value!";;

// ----------------------------------------------------------------------------
// Section 5.4 Function values

// Listing 5.14 Filtering using predicate

let nums = [ 4; 9; 1; 8; 6 ]
// Filtering using predicate
let evens = List.filter (fun n -> n%2 = 0) nums 


// Listing 5.15 Using lambda functions and let bindings

// Square written using let binding
let square1(a) = a * a
// Square written using lambda notation
let square2 = fun a -> a * a

// Adding two integers
let add = fun a b -> a + b
// Calling the function
add 2 3


// Listing 5.16 Advanced lambda functions 

let sayHello = 
  // Complex lambda function is enclosed in braces
  // Using F# type annotation
  (fun (s:string) ->
    let msg = String.Format("Hello {0}!", s)     
    Console.WriteLine(msg)                       
  )  

// ----------------------------------------------------------------------------
// Section 5.4.2: Function type    

// Listing 5.17 Function as an argument in F#

// F# function taking function as an argument
let twice n (f:int -> int) = f(f(n))

// Calling using lambda function
twice 2 (fun n -> n * n)
    

// Listing 5.18 Function as a return value in F#
let adder(n) =     
    // Create return value using lambda function syntax
    (fun a -> a + n)

// Call function that returns a function
let addTen = adder 10
// Invoke the returned function
addTen 15


// Partial function application
module Example2 = 

  let add a b = a + b
  // Create a function that adds 10 to any given number
  let addTen = add 10
  // Add 15 to all numbers in a list
  List.map (add 10) [ 1 .. 10 ]
