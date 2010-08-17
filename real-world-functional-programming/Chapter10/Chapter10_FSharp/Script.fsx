#light

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 10
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 10.1.1 Avoid stack overflow using tail recursion

// Listing 10.1 Summing list and stack overflow (F# interactive)
module Listing_10_1 =
  // Create lists for testing
  let test1 = [ 1 .. 10000 ]
  let test2 = [ 1 .. 1000000 ]

  let rec sumList(lst) =
      match lst with
      | [] -> 0 // Branch that returns immediately 
      | hd::tl -> hd + sumList(tl) // Recursive branch

  // Stack size is sufficient
  sumList(test1)
  // Too many nested function calls!
  sumList(test2)


// Listing 10.2 Tail-recursive version of the 'sumList' function
let rnd = new System.Random()
let test1 = List.init 10000 (fun _ -> rnd.Next(101) - 50)
let test2 = List.init 1000000 (fun _ -> rnd.Next(101) - 50)

module Listing_10_2 =
  
  let sumList(lst) =
    // Private function with the accumulator 'total'
    let rec sumListUtil(lst, total) = 
      match lst with 
      | [] -> total // Return the accumulated value 
      | hd::tl -> 
          // Add current value to accumulator 
          let ntot = hd + total
          // Recursive call
          sumListUtil(tl, ntot)
    // Calls the helper with total = 0
    sumListUtil(lst, 0)

  // Both calls compute the result now!
  sumList(test1)
  sumList(test2)

// --------------------------------------------------------------------------
// Section 10.1.2 Caching results using memoization

// Listing 10.3 Adding numbers with memoization
open System.Collections.Generic

module Listing_10_3 = 
  // Non-optimized addition
  let rec addSimple(a, b) = 
    // Prints info for debugging 
    printfn "adding %d + %d" a b
    a + b

  // Addition optimized using memoization
  let add = 
    // Initialize the cache 
    let cache = new Dictionary<_, _>()
    // Created function uses the private cache
    (fun x ->
        // Read the value from the cache or calculate it
        match cache.TryGetValue(x) with
        | true, v -> v
        | _ -> let v = addSimple(x)
               cache.Add(x, v)
               v)

  // Calls the 'addSimple' function
  add(2, 3)

  // Value is obtained from the cache
  add(2, 3)


// --------------------------------------------------------------------------
// Section Reusable memoization

// Listing 10.5 Generic memoization function
let memoize(f) =    
  // Initialize cache captured by the closure
  let cache = new Dictionary<_, _>()
  (fun x ->
      let succ, v = cache.TryGetValue(x)
      if succ then v else 
        let v = f(x)
        cache.Add(x, v)
        v)

// Listing 10.6 Difficulties with memoizing recursive function
module Listing_10_6 =

  // Standard recursive factorial
  let rec factorial(x) =
    printf "factorial(%d); " x
    if (x <= 0) then 1 else x * factorial(x - 1) // Recursive call

  // Memoize it using 'memoize' function
  let factorialMem = memoize(factorial)

  // Calculate 2! for the first time
  factorialMem(2)
  // Use the cached value
  factorialMem(2)
  // The value of 2! is being recalculated!!
  factorialMem(3)


// Listing 10.7 Correctly memoized factorial function
module Listing_10_7 =
  let rec factorial = memoize(fun x ->
    printf "factorial(%d); " x
    if (x <= 0) then 1 else x * factorial(x - 1)) // Recursive reference to the 'factorial' value
  
  // Compute first few values
  factorial(2)
  // Compute only the missing value
  factorial(4)
  
// --------------------------------------------------------------------------
// Section 10.2 Working with lists efficiently

// Listing 10.8 Naïve list processing functions

let rec mapN f ls =
  match ls with
  | [] -> []
  | x::xs -> 
      f(x) :: (mapN f xs)

let rec filterN f ls =
  match ls with
  | [] -> []
  | x::xs -> 
      let xs = (filterN f xs)
      if f(x) then x::xs else xs

// Listing 10.9 Tail recursive list processing functions

let map f ls =
  let rec map' f ls acc =
    match ls with
    | [] -> acc
    | x::xs -> map' f xs (f(x) :: acc)
  map' f ls [] |> List.rev

let filter f ls =
  let rec filter' f ls acc =
    match ls with
    | [] -> acc
    | x::xs -> 
      filter' f xs 
        (if f(x) then x::acc else acc)        
  filter' f ls [] |> List.rev

// Testing the naive and tail-recursive version..
let large = [ 1 .. 100000 ]

// Tail recursive function works fine
large |> map (fun n -> n*n)
// Non-tail recursive function causes stack overflow
large |> mapN (fun n -> n*n)

// --------------------------------------------------------------------------
// Section 10.2.2 Efficiency of list functions

// Listing 10.10 Adding elements to a list (F# interactive)

// Appends simply using cons operator
let appendFront el list = el::list

// Appends to the end using a recursive function
let rec appendEnd el list =
  match list with
  | []    -> [el] // Append to an empty list
  | x::xs -> x::(appendEnd el xs) // Recursive call append to the tail

let l = [ 1 .. 30000 ]

// Turns on time measuring in F# interactive
#time 
// Executing 'appendFront' 100x takes almost no time
for i = 1 to 100 do ignore(appendFront 1 l)
// 100x 'appendEnd' takes roughly 1 second
for i = 1 to 100 do ignore(appendEnd 1 l);;
// Turn off time measuring
#time 

// --------------------------------------------------------------------------
// Section 10.3 Working with arrays 

// Listing 10.11 Creating and using arrays (F# interactive)

// Initialize an array with 5 elements
let arr = [| 1 .. 5 |]
// Change the value at the specified index
arr.[2] <- 30;;
// Imperative code to sum the elements
let mutable sum = 0
for i in 0 .. 4 do
    sum <- arr.[i] + sum;;


// Listing 10.12 Functional way of working with arrays (F# interactive)

// Initialize array using the given function
let numbers = Array.init 5 (fun _ -> rnd.Next(10))
// Calculate new element from the original
let squares = numbers |> Array.map (fun n -> (n, n*n))
// Print tuples from the resulting array
for sq in squares do
  printf "%A " sq


// Listing 10.14 Functional implementation of blur for arrays (F#)

// Type annotation, so we can use "Length" member
let blurArray (arr:int[]) =
  // Initialize empty result
  let res = Array.create arr.Length 0
  // Calculate value at borders
  res.[0] <- (arr.[0] + arr.[1]) / 2
  res.[arr.Length-1] <- (arr.[arr.Length-2] + arr.[arr.Length-1]) / 2
  
  for i in 1 .. arr.Length - 2 do
    // Calculate average over 3 elements
    res.[i] <- (arr.[i-1] + arr.[i] + arr.[i+1]) / 3
  res


// Test the function above...

// Initialize random array
let ar = Array.init 10 (fun _ -> rnd.Next(20));;
// Blur the array once
ar |> blurArray
// Blur three times using pipelining
ar |> blurArray |> blurArray |> blurArray


// --------------------------------------------------------------------------
// Section 10.3 Introducing continuations

// Listing 10.14 Tree data structure and summing elements (F# interactive)

// Tree is a leaf with value or a node containing sub-trees
type IntTree =
  | Leaf of int
  | Node of IntTree * IntTree

// Recursive function calculating sum of elements
let rec sumTree(tree) =
  match tree with
  // Sum of a leaf is its value
  | Leaf(n)    -> n
  // Recursively sum values in the sub-trees
  | Node(l, r) -> sumTree(l) + sumTree(r)

let tree = 
  Node(Node(Node(Leaf(5), Leaf(8)), Leaf(2)),
       Node(Leaf(2), Leaf(9)));;
sumTree(tree)

// Deep recursion causes stack overflow!
let imbalancedTree =
  test2 |> List.fold (fun st v ->
      // Add node with previous tree on the right
      Node(Leaf(v), st)) (Leaf(0))
sumTree(imbalancedTree)

// --------------------------------------------------------------------------
// Section 10.3.1 Writing code using continuations

// Listing 10.16 Sum elements of a tree using continuations (F# interactive)

// Return value by calling the continuation
let rec sumTreeCont tree cont =
  match tree with
  | Leaf(n)    -> cont(n)
  | Node(left, right) -> 
      // Recursively sum left sub-tree
      sumTreeCont left (fun leftSum ->
        // Then recursively sum right sub-tree
        sumTreeCont right (fun rightSum ->
          // Finally, call the continuation with the result
          cont(leftSum + rightSum)))
          
// Print the result inside the continuation
sumTreeCont imbalancedTree (fun r ->
  printfn "Result is: %d" r)

// Returning sum from the continuation
sumTreeCont imbalancedTree (fun a -> a)