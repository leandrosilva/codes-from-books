//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicAggregation.Main

open System
open System.Threading.Tasks

open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.BasicAggregation

// ------------------------------------------------------------------------------
// Run all basic aggregation samples
// ------------------------------------------------------------------------------

/// The length of the sample sequence to be processed
let sequenceSize = 100000000

let main() =
    Console.WriteLine("Basic Aggregation Samples\n")
#if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n")
#endif
    let sequence = SampleUtilities.Range(sequenceSize)

    SampleUtilities.TimedAction 
      "calculate sum, sequential for loop" 
      (fun () -> Examples.Chapter4Sample01Sequential(sequence)) |> ignore

    SampleUtilities.TimedAction 
      "calculate sum, sequential, functional using recursion" 
      (fun () -> Examples.Chapter4Sample01SequentialFunctional(sequence)) |> ignore
    
    GC.Collect()
    SampleUtilities.TimedAction 
      "calculate sum, incorrectly coded parallel loop" 
      (fun () -> Examples.Chapter4Sample01IncorrectParallel(sequence)) |> ignore
    
    GC.Collect()
    SampleUtilities.TimedAction 
      "calculate sum, LINQ (sequential) "
      (fun () -> Examples.Chapter4Sample02Linq(sequence)) |> ignore
    
    GC.Collect()
    SampleUtilities.TimedAction 
      "calculate sum, PLINQ (parallel) "
      (fun () -> Examples.Chapter4Sample02Plinq(sequence)) |> ignore

    GC.Collect()
    SampleUtilities.TimedAction 
      "calculate sum, F# PSeq (parallel) "
      (fun () -> Examples.Chapter4Sample02PSeq(sequence)) |> ignore
    
    GC.Collect()
    SampleUtilities.TimedAction 
      "custom aggregation (product) PLINQ (parallel) "
      (fun () -> Examples.Chapter4Sample03Plinq(sequence)) |> ignore

    GC.Collect()
    SampleUtilities.TimedAction 
      "custom aggregation (product) F# PSeq (parallel) "
      (fun () -> Examples.Chapter4Sample03PSeq(sequence)) |> ignore
    
    GC.Collect()             
    SampleUtilities.TimedAction 
      "calculate sum, parallel for each" 
      (fun () -> Examples.Chapter4Sample01Parallel(sequence)) |> ignore

    GC.Collect()
    SampleUtilities.TimedAction 
      "calculate sum, parallel partitions" 
      (fun () -> Examples.Chapter4Sample01ParallelPartitions(sequence)) |> ignore

    Console.WriteLine("\nRun complete... press enter to finish.")
    Console.ReadLine()

// ------------------------------------------------------------------------------
// Run the main testing program in the thread pool

do
    Task.Factory.StartNew(main).Wait()
