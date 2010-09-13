//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicDynamicTasks.Main

open System
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.BasicDynamicTasks.Examples


let n = 1                // number of timing runs per test
let time = 0.01          // CPU time in seconds to visit each node of the tree
let treeSize = 2000      // number of nodes in the tree
let treeDensity = 0.75   // P(left/right child node exists) for interior nodes


// Generate a sample tree and process it using various algorithms
let main() =
    Console.WriteLine("Basic Dynamic Task Samples\n")
#if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n")
#endif
    Console.WriteLine("Tree Walking")

    // Initialize tree
    let tree = makeTree treeSize treeDensity

    // Process the tree using various algorithms
    SampleUtilities.TimedAction "tree traversal, sequential" (fun () -> 
        Chapter6.Example1Sequential n time tree ) |> ignore
    SampleUtilities.TimedAction "tree traversal, parallel" (fun () -> 
        Chapter6.Example1Parallel n time tree ) |> ignore
    SampleUtilities.TimedAction "tree traversal, parallel - attached to parent" (fun () ->
        Chapter6.Example1ParallelAttached n time tree ) |> ignore
    SampleUtilities.TimedAction "parallel while not empty - Parallel.ForEach" (fun () ->
        Chapter6.Example01ParallelWhileNotEmpty n time tree ) |> ignore
    SampleUtilities.TimedAction "parallel while not empty - parallel tasks" (fun () ->
        Chapter6.Example01ParallelWhileNotEmpty2 n time tree ) |> ignore

    // F# specific - Traverse tree in parallel using F# asynchronous workflows
    SampleUtilities.TimedAction "tree traversal, parallel - asynchronous workflows " (fun () -> 
        Chapter6.Example1ParallelAsync n time tree ) |> ignore

    Console.WriteLine("\nRun complete... press enter to finish.")
    Console.ReadKey() |> ignore


// Start the main function as a task
do 
    Task.Factory.StartNew(main).Wait()
