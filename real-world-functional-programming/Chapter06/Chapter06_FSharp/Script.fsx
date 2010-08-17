#light

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 6
// --------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Section 6.1.1: Writing generic functions

// Listing 6.1: Generic function 'condPrint'

// Function with three arguments
let condPrint value test format =
    // Calling functions given as arguments
    if (test(value)) then printfn "%s" (format(value))

// Test the function
condPrint 10 (fun n -> n > 5)
             (fun n -> "Number: " + n.ToString());;


// ----------------------------------------------------------------------------
// Section 6.1.2: Custom operators

// Listing 6.2: Working with strings using custom operator

// Operator for concatenating strings in a special way
let (+>) a b = a + "\n>>" + b
// Concatenate several messages
printfn "%s" ("Hello world!" +>
              "How are you today?" +>
              "I'm fine!")


// ----------------------------------------------------------------------------
// Section 6.2: Working with tuples

// Listing 6.4: High-order functions for working with tuples

// Applies function to the first element
let mapFirst  f (a, b) = (f(a), b)
// Applies function to the second element
let mapSecond f (a, b) = (a, f(b))

// Listing 6.5: Working with tuples

let oldPrague = ("Prague", 1188000)

// Using lambda function as an argument
mapSecond (fun n -> n + 13195) oldPrague

// Using partial application and pipelining
oldPrague |> mapSecond ((+) 13195)

// ----------------------------------------------------------------------------
// Section 6.3.1: Calculating with schedules

// Type declaration from chapter 5:
open System
type Schedule =
    | Never
    | Once of DateTime
    | Repeatedly of DateTime * TimeSpan


// Listing 6.7: Map operation for schedule type

let mapSchedule rescheduleFunc schedule =
    match schedule with
    | Never ->
        // Unscheduled events remain unscheduled
        Never
    | Once(eventDate) ->
        // Reschedule event occurring once
        Once(rescheduleFunc(eventDate))
    | Repeatedly(startDate, interval) ->
        // Reschedule repeated event
        Repeatedly(rescheduleFunc(startDate), interval)

// Listing 6.8: Rescheduling using 'mapSchedule' function

// Create list of schedules for testing
let schedules =
    [ Never; Once(DateTime(2008, 1, 1));
      Repeatedly(DateTime(2008, 1, 2), TimeSpan(24*7, 0, 0)) ];;

for s in schedules do
    // Add one week using 'mapSchedule'
    let ns = s |> mapSchedule (fun d -> d.AddDays(7.0))
    // Print the new schedule
    printfn "%A" ns

// Processing list of schedules

let newSchedules1 =
    List.map (fun s ->
        let ns = s |> mapSchedule (fun d -> d.AddDays(7.0))
        ns ) schedules

// Using partial function application
let newSchedules2 = schedules |> List.map (mapSchedule (fun d -> d.AddDays(7.0))) 


// ----------------------------------------------------------------------------
// Section 6.3.2: Working with option type

// 'readInput' function from chapter 5
let readInput() =
    let (succ, num) = Int32.TryParse(Console.ReadLine())
    if (succ) then Some(num) else None


// Listing 6.10: Adding two options using pattern matching

let readAndAdd1() =
    match (readInput()) with
    | None    -> None
    | Some(n) ->         // Extract value from the first input
        match (readInput()) with
        | None    -> None
        | Some(m) ->     // Extract value from the second input
            Some(n + m)  // Add numbers and return the result


// Listing 6.11: Adding two options using bind and map

let readAndAdd2() =
    readInput() |> Option.bind (fun num ->     // Process first input using 'bind'
        readInput() |> Option.map ((+) num) )  // Process second input using 'map'

// Listing 6.12: Implementing bind and map

let bind f opt =
    match opt with
    | Some(v) -> f(v)   // Call the function
    | None -> None

let map f opt =
    match opt with
    | Some(v) -> Some(f(v))   // Call the function and wrap the result
    | None -> None


// ----------------------------------------------------------------------------
// Section 6.4.1: Function composition

// Listing 6.13 Working with city information

// Create list with testing settlements
let places1 = [ ("Grantchester", 552); ("Cambridge", 117900); ("Prague", 1188126); ]

// Returns status based on the population
let statusByPopulation(population) =
    match population with
    | n when n > 1000000 -> "City"
    | n when n >    5000 -> "Town"
    | _                  -> "Village"

// Iterate over settlements and read population information
places1 |> List.map (fun (_, population) ->
    // Calculate the status
    statusByPopulation(population))


// Listing 6.14 Using function composition operator

// Calling composed function explicitly
places1 |> List.map (fun x -> (snd >> statusByPopulation) x)
// Using composed function as an argument
places1 |> List.map (snd >> statusByPopulation)

// Implementation of the function composition operator
let (>>) f g x = g(f(x))


// ----------------------------------------------------------------------------
// Section 6.6: Working with lists

// Listing 6.17: Definition of a functional list type (F#)

// The type is generic
type List<'a> =
    | Nil                     // Represents an empty list
    | Cons of 'a * List<'a>   // List with head and a tail

// Create list containing 1, 2, 3
let list = Cons(1, Cons(2, Cons(3, Nil)));;


// Listing 6.19: Data about settlements

let places =
  [ ("Seattle", 594210);
    ("Prague", 1188126);
    ("New York", 7180000);
    ("Grantchester", 552);
    ("Cambridge", 117900); ]

// Filtering and projection using ordinary function calls
let names1 =
    List.map fst
             (List.filter (fun (_, pop) -> 1000000 < pop) places)

// Filtering & projection using pipelining
let names2 =
    places |> List.filter (fun (_, pop) -> 1000000 < pop)
           |> List.map fst

// We can rewrite the first function using function composition
let names3 =
    places |> List.filter (snd >> ((<) 1000000))
           |> List.map fst


// Listing 6.20: Generic list aggregation

let rec fold f init list =
    match list with
    | []     -> // Return initial value
                init
    | hd::tl -> // Update state using the first element
                let state = f init hd
                // Recrusively process the rest
                fold f state tl


// Listing 6.21: Examples of using FoldLeft operation

// Sum population into 'int'
places |> List.fold (fun sum (_, pop) -> sum + pop) 0;;

// Format into a 'string'
places |> List.fold (fun s (n, _) -> s + n + ", ") "";;

// Aggregation using 'bool * string' value
places
    |> List.fold (fun (b,str) (name, _) ->
         let n = if b then name.PadRight(20) else name + "\n"
         (not b, str+n)
     ) (true, "")  // Initial tuple value
  |> snd           // Drop the helper element from a tuple
  |> printfn "%s"  // Print the formatted string


// ------------------------------------------------------------------
// Section 6.7: Common processing language

open System.IO

let path = @"C:\Source"
 
let directories = 
   [ path + "\Chapter06_CSharp"; 
     path + "\Chapter06_FSharp"; 
     path + "\FunctionalCSharp" ]

directories |> List.collect (fun d ->
  d |> Directory.GetFiles
    |> List.ofSeq                
    |> List.map Path.GetFileName )