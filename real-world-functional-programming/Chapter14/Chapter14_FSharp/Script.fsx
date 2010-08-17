// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 14
// --------------------------------------------------------------------------
// This file demonstrates techniques that are used in sample applications
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// 14.1.1 Parallelizing islands of imperative code

let inp = [| 1; 6; 0; 2; 4; 2; 8; 5; 1; 2 |]
let res = Array.create 10 0

// Listing 14.1 For loop for calculating blurred array
for i in 1 .. inp.Length - 2 do
  let sum = inp.[i-1] + inp.[i] + inp.[i+1]
  res.[i] <- sum / 3                

// Utility function for writing parallel for loops
#r "System.Core.dll"

// For Visual Studio 2008, download "Parallel Extensions to .NET" 
// and add reference to the System.Threading.dll assembly
// #r "System.Threading.dll"

open System
open System.Threading.Tasks

let pfor nfrom nto f = 
  Parallel.For(nfrom, nto + 1, Action<_>(f)) |> ignore

// Listing 14.2 Parallelized for loop (C# and F#)
pfor 1 (inp.Length-2) (fun i ->
    let sum = inp.[i-1] + inp.[i] + inp.[i+1]
    res.[i] <- sum / 3
  )       


// --------------------------------------------------------------------------
// 14.1.2 Declarative data parallelism

// Listing 14.3 Counting the number of primes (C# and F#)
let isPrime(n) =
  let ms = int(sqrt(float(n)))
  let rec isPrimeUtil(m) =
    if m > ms then true
    elif n % m = 0 then false
    else isPrimeUtil(m + 1)
    
    // Side Note:
    // The same thing can be written more compactly like this:
    //   m >= ns || (n % m <> 0 && (isPrimeUtil(m+1)))
    
  (n > 1) && isPrimeUtil(2)

// Count the primes
let nums = [1000000 .. 2999999]

// Turn the timing On
#time

let primeCount1 =
  nums |> List.filter isPrime
       |> List.length

// Load the parallel module       
#load "..\parallel.fs"       
open Functional

// Lising 14.4 Counting primes in parallel (C# and F#)
let primeCount2 =
  nums |> PSeq.ofSeq
       |> PSeq.filter isPrime
       |> PSeq.length

// Turn the timing Off
#time

// Listing 14.5 Parallelizing sequence expressions
seq {
  for n in nums do
    if (isPrime n) then 
          yield n } 
  |> Seq.length

// Using parallel sequence expression from the 'Parallel' module
pseq {
  for n in nums do
    if (isPrime n) then 
      yield n } 
  |> PSeq.length


// --------------------------------------------------------------------------
// Section 14.1.2 Task-based parallelism

// Listing 14.6 Counting primes in a binary tree

// Binary tree type as in chapter 10
// Either a leaf with value or a node containing sub-trees
type IntTree =
  | Leaf of int
  | Node of IntTree * IntTree

let rnd = new Random()

// Generates random tree with the specified depth
let rec tree(depth) =
  if depth = 0 then Leaf(rnd.Next())
  else 
    // Recursively generate sub-trees
    Node(tree(depth-1), tree(depth-1))

// Count prime numbers in the tree
let rec count(tree) =
  match tree with
  | Leaf(n) when isPrime(n) -> 1 // Return 1 when number is prime and 0 otherwise
  | Leaf(_) -> 0
  | Node(l, r) -> 
      // First process the left sub-tree, then the right one
      count(l) + count(r)


// Extension that makes the 'Future' type nicely useable from F#
open System.Threading.Tasks

// Listing 14.7 Parallel processing of a binary tree
let pcount(tree) =
  // Implementation function that also counts the depth
  let rec pcountDepth(tree, depth) =
    match tree with
    | _ when depth >= 5 -> count(tree) // Use sequential version for small sub-problems
    | Leaf(n) when isPrime(n) -> 1
    | Leaf(_) -> 0
    | Node(l, r) ->
        // Create 'Future' to process the left sub-tree
        let cl = Task.Factory.StartNew(fun() ->
          pcountDepth(l, depth+1))
        // Process the right sub-tree
        let cr = pcountDepth(r, depth+1)
        // Wait for both of the results and add them
        cl.Result + cr
  pcountDepth(tree, 0)

// Measure the performance
#time

let t = tree(16)
count(t)
pcount(t)
