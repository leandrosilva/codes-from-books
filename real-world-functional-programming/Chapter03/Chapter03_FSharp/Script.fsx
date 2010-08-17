// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 3
// --------------------------------------------------------------------------
// NOTE: We're using the 'do' keyword in the source file to separate 
// individual listings. It creates a local scope, so we don't have to worry
// about using same symbol name multiple times (e.g. 'number' in 
// the first two listings)
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 3.1.1: Value declarations and scope

// Listing 3.1: The scope of a value
do
  let number = 42
  printfn "%d" number
  let message = "Answer: " + (number.ToString())
  printfn "%s" message

// Listing 3.2: Example with let binding with explicit scopes
do
  let number = 42 in
  (
    printfn "%d" number;
    let message = 
      "Answer: " + (number.ToString()) in
    (
      printfn "%s" message
    )
  )

// --------------------------------------------------------------------------
// Section 3.1.2: Function declarations

let multiply num1 num2 =
    num1 * num2

// Listing 3.3: Nested let bindings (in F# interactive)
let printSquares msg n1 n2 =
    let printSqUtility n =
        let sq = n * n
        printfn "%s %d: %d" msg n sq
    printSqUtility n1
    printSqUtility n2

printSquares "Square of" 14 27

// --------------------------------------------------------------------------
// Section 3.1.2: Declaring mutable values

// Listing 3.4: Declaring mutable values
let n1 = 22

// error FS0027: This value is not mutable.
// n1 <- 23

let mutable n2 = 22
n2 <- 23

// --------------------------------------------------------------------------
// Section 3.2: Using immutable data structures

// Listing 3.5: Working with tuples in F# (F# interactive)
// Function that prints information about the city
let printCity(cityInfo) =
    printfn "Population of %s is %d."
            (fst cityInfo) (snd cityInfo)

do
  // Create tuples representing Prague and Seattle
  let prague  = ("Prague", 1188126)
  let seattle = ("Seattle", 594210)

  // Print information about the cities
  printCity(prague)
  printCity(seattle)

  // Output:
  //   Population of Prague is 1188126.
  //   Population of Seattle is 594210.

// ----------------------------------------------------------------------------
// Section 3.2.3: Calculating with tuples

// Listing 3.10: Incrementing population of a city in F#
do
  let withSecond tuple newItem2 =
      let (originalItem1, originalItem2) = tuple // Decompose a tuple into two values
      (originalItem1, newItem2) // Use 'f' as a first and 'nsnd' as the second element

  // Increment population and print the new information
  let prague0 = ("Prague", 1188000)
  let prague1 = withSecond prague0 ((snd prague0) + 13195)
  printCity prague1

// ----------------------------------------------------------------------------
// Section 3.2.4: Pattern matching with tuples

// Listing 3.11 Pattern matching with multiple patterns
let withSecond tuple nsnd =
  match tuple with
  // Pattern that matches only New York
  | ("New York", _) -> ("New York", nsnd + 100)
  // Pattern that matches all other values
  | (f, _) -> (f, nsnd)

// The expected result for Prague is '10'
let prague = ("Prague", 123)
withSecond prague 10

// Returned population is incremented by 100 (that is 110)
let ny = ("New York", 123)
withSecond ny 10

// ----------------------------------------------------------------------------
// Section 3.3.1: Recursive computations

// Listing 3.12 Factorial in both C# and F#

// Declaration of recursive function or method; In F# we have to explicitly
// declare that it is recursive by using the 'let rec' binding
// instead of ordinary 'let'
let rec factorial n =
    if (n <= 1) then
        // A case which terminates the recursion and returns 1 immediately
        1
    else
        // A case which performs the recursive call to a 'factorial' function
        n * (factorial (n-1))

// ----------------------------------------------------------------------------
// Section 3.3.2 Introducing functional lists
let ls1 = []
let ls2 = 6::2::7::3::[]
let ls3 = [6; 2; 7; 3]
let ls4 = [1 .. 5]
let ls5 = 0::ls4

// Listing 3.13: An incomplete pattern matching on lists (F# interactive)
let squareFirst l =
    match l with
    | hd::_ -> hd * hd

// F# detects possible failure:
//   Warning FS0025: Incomplete pattern matches on this
//   expression. The value '[]' will not be matched.

squareFirst [4; 5; 6]
// Success for a non-empty list:
//   val it : int = 16

squareFirst []
// Failure for an empty list
//   Exception of type 'Microsoft.FSharp.Core. MatchFailureException' was thrown.

// ----------------------------------------------------------------------------
// Section 3.3.4: Functional list processing

// Listing 3.16 Sum of list elements (F# interactive)
let rec sumList lst =
    // Pattern matching on the list
    match lst with
    | []         ->   0           // Sum of empty list is 0
    | hd::tl -> hd + (sumList tl) // A branch for a cons cell

// Create list for testing
let list = [ 1 .. 5 ]
// Calculate the sum and print it
sumList list

// ----------------------------------------------------------------------------
// Section 3.4: Using functions as values

// Listing 3.18 Adding and multiplying list elements (F# interactive)
let rec aggregateList (f:int -> int -> int) init list =
    match list with
    | []     -> init                     // Empty list branch
    | hd::tl ->
       let rem = aggregateList f init tl // Non-empty list branch
       (f rem hd)

// Functions for addition and multiplication
let add a b = a + b
let mul a b = a * b

// Test the function immediately  
aggregateList add 0 [ 1 .. 5 ]
aggregateList mul 1 [ 1 .. 5 ]


// The same thing written directly using F# operator:
aggregateList (+) 0 [ 1 .. 5 ];;

// We can also use 'aggregateList' to find maximum of the list:
aggregateList max (-1) [ 4; 1; 5; 2; 8; 3 ];;