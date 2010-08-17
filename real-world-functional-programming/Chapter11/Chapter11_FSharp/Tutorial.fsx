#light

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 11
// --------------------------------------------------------------------------
// This source file contains all the examples with exception of
// the larger 'photos' application that uses lazy values to cache\
// resized photo previews
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 11.1 Refactoring functional programs
// Section 11.1.1 Reusing common code blocks

let loadPlaces() =
  [ ("Seattle", 594210);   ("Prague", 1188126)
    ("New York", 7180000); ("Grantchester", 552)
    ("Cambridge", 117900) ]
 
// Listing 11.1 Printing information about places
   
let printBigCities() =
  let places = loadPlaces()
  printfn "===== Big cities ====="
  let sel = List.filter (fun (_, s) -> s > 1000000) places
  for n, p in sel do
    printfn " - %s, (%d)" n p

let printAllByName() =
  let places = loadPlaces()
  printfn "===== Sorted by name ====="
  let sel = List.sortBy fst places
  for n, p in sel do
    printfn " - %s, (%d)" n p

printBigCities()
printAllByName()

// Listing 11.2 Reusable function for printing information

let printPlaces title select =
  let places = loadPlaces()
  // Title is passed as an argument
  printfn "== %s ==" title
  // Strategy for processing places is a function
  let sel = select(places)
  for n, p in sel do
    printfn " - %s, (%d)" n p

// Listing 11.3 Working with 'printPlaces' function

// Printing only big cities
printPlaces "Big cities" (fun places ->  
    List.filter (fun (_, s) -> s > 200000) places)

// The same call using partial function application
printPlaces "Big cities" (List.filter (fun (_, s) -> s > 200000))
printPlaces "Sorted by name" (List.sortBy fst)

// --------------------------------------------------------------------------
// Section 11.3.3 Lazy values in F#

// Listing 11.15 Introducing lazy values

// Function with side effect that we can track
let foo(n) = 
   printfn "foo(%d)" n
   n < 10

// Create delayed computation using 'lazy' keyword
let n = lazy foo(10)
// 'foo' gets called and the result is returned
n.Value
// Result is returned immediately this time!
n.Value

// Listing 11.16 Comparing eager and lazy "or" operator

// Eager 'or' operator that takes arguments as simple values
let (||!) a b = 
   if a then true
   elif b then true
   else false

// Lazy 'or' operator that takes arguments as lazy values
let (||?) (a:Lazy<_>) (b:Lazy<_>) = 
   if a.Value then true
   elif b.Value then true
   else false

// Using the eager one - both side-effects are executed
if (foo(5) ||! foo(7)) then 
   printfn "True"

// Using the lazy version - only first function is called
if (lazy foo(5) ||? lazy foo(7)) then 
    printfn "True"



// --------------------------------------------------------------------------
// Section 11.4.1 Introducing infinite data structures

// Listing 11.18 Infinite list of integers (F#)
type InfiniteInts =
  | LazyCell of 
      int *               // Value in the current cell
      Lazy<InfiniteInts>  // Next cell is a delayed computation


// Listing 11.19 Creating a list containing 0, 1, 2, 3, 4, ...

// Create an infinite list starting from 'num'
let rec numbers(num) =
  // Store first value in the cell
  LazyCell
    (num, 
     // The next cell is a delayed computation
     lazy numbers(num + 1))
  
// Get all numbers from zero
numbers(0)

// Returns the next cell of the list
let next (LazyCell(hd, tl)) =
  // Evaluate the lazy value representing the next cell
  tl.Value

// Access sixth value from the list
numbers(0) |> next |> next |> next |> next |> next

// Apply the specified function to 'all' elements
let rec map f (LazyCell(hd, tl)) =
   LazyCell(f hd, lazy map f tl.Value);;

// Calculate squares and get the sixth square
let squares = numbers(0) |> map (fun n -> n * n)
squares |> next |> next |> next |> next |> next