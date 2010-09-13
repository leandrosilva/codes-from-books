//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops.Examples

open System
open System.Collections.Concurrent
open System.Linq
open System.Threading.Tasks
open Microsoft.FSharp.Collections
open Microsoft.Practices.ParallelGuideSamples.Utilities

// ------------------------------------------------------------------------------
// Type declarations
// ------------------------------------------------------------------------------

/// Stores configuration of a sample run such as number of steps etc.
type ExampleOptions = 
  { LoopBodyComplexity : int
    NumberOfSteps : int
    VerifyResult : bool
    SimulateInternalError : bool }

/// Thrown to simulate an error in a parallel loop
exception ParallelForExampleException 


// --------------------------------------------------------------------------
// Work Function
// --------------------------------------------------------------------------

let private doWorkExpectedResult opts i =
    2.5 * (float opts.LoopBodyComplexity + 1.0) 
        * (float opts.LoopBodyComplexity) * (float i)

let private doWork opts (i:int) = 
    let mutable result = 0.0
    for j in 1 .. opts.LoopBodyComplexity do
        let j2 = float j
        let i2 = float i
        result <- result + Math.Sqrt((9.0 * i2 * i2 + 16.0 * i2 * i2) * j2 * j2)

    // Simulate unexpected condition in loop body
    if i % 402030 = 2029 && opts.SimulateInternalError then
        raise ParallelForExampleException
    
    Math.Round(result)

let ensureResult opts i actual =
    let expected = doWorkExpectedResult opts i
    if actual <> expected then
        let msg = sprintf "Unexpected value: actual %f, expected %f" actual expected
        raise (new InvalidOperationException(msg))

// ------------------------------------------------------------------------------
// Main module that contains worker functions and examples
// ------------------------------------------------------------------------------

module Chapter2 = 

    // --------------------------------------------------------------------------
    // For Examples
    // --------------------------------------------------------------------------

    /// Sequential for loop
    let Example01 opts =
        let result = Array.zeroCreate opts.NumberOfSteps
        for i in 0 .. opts.NumberOfSteps - 1 do 
            result.[i] <- doWork opts i
        result


    /// LINQ 1 (sequential)
    let Example01b opts = 
        Enumerable.Range(0, opts.NumberOfSteps).Select(doWork opts).ToArray()


    /// Equivalent using F# libraries (sequential)
    let Example01c opts = 
        [| 0 .. opts.NumberOfSteps - 1 |]
        |> Array.map (doWork opts)


    /// Parallel for loop
    let Example02 opts = 
        let result = Array.zeroCreate opts.NumberOfSteps
        Parallel.For(0, opts.NumberOfSteps, fun i ->
            result.[i] <- doWork opts i) |> ignore
        result

    
    /// Parallel LINQ
    let Example03 opts =
        let result = Array.zeroCreate opts.NumberOfSteps
        ParallelEnumerable
          .Range(0, opts.NumberOfSteps)
          .ForAll(fun i -> result.[i] <- doWork opts i )
        result


    /// Parallel LINQ (using query operators)
    let Example04 opts = 
        ParallelEnumerable
          .Range(0, opts.NumberOfSteps)
          .AsOrdered()
          .Select(doWork opts).ToArray()


    /// Parallel LINQ (using F# module PSeq)
    let Example05 opts = 
        PSeq.ofList [ 0 .. opts.NumberOfSteps ]
          |> PSeq.ordered
          |> PSeq.map (doWork opts)
          |> PSeq.toArray
                  

    /// Optimized for small units of work, each of the same duration avoids false sharing
    /// (not appropriate if iteration steps of unequal duration)
    let Example06 opts =
        let result = Array.zeroCreate opts.NumberOfSteps
        Parallel.ForEach(Partitioner.Create(0, opts.NumberOfSteps), fun (min, max) ->
            for i in min .. max - 1 do
                result.[i] <- doWork opts i ) |> ignore
        result


    /// Split the work into partitions with the user-specified range size
    /// (not appropriate if iteration steps of unequal duration)
    let Example07 opts =
        let result = Array.zeroCreate opts.NumberOfSteps
        let rangeSize = opts.NumberOfSteps / (Environment.ProcessorCount * 10)
        let partitions = Partitioner.Create(0, opts.NumberOfSteps, if rangeSize >= 1 then rangeSize else 1)
        Parallel.ForEach(partitions, fun (min, max) ->
            for i in min .. max - 1 do
                result.[i] <- doWork opts i ) |> ignore
        result

    // --------------------------------------------------------------------------
    // ForEach Examples
    // --------------------------------------------------------------------------
        
    /// Sequential for loop
    let Example21 opts (source:_[]) =
        let result = Array.zeroCreate source.Length
        for i in source do
            result.[i] <- doWork opts i
        result


    /// LINQ 1 (sequential)
    let Example21b opts (source:_[]) = 
        source.Select(doWork opts).ToArray()


    /// Equivalent using F# libraries (sequential)
    let Example21c source opts=
        source |> Array.map (doWork opts)


    /// Parallel foreach loop
    let Example22 opts (source:_[]) =
        let result = Array.zeroCreate source.Length
        Parallel.ForEach(source, fun i -> 
            result.[i] <- doWork opts i ) |> ignore
        result


    /// Parallel LINQ
    let Example23 opts (source:_[]) = 
        let result = Array.zeroCreate source.Length
        source
          .AsParallel()
          .ForAll(fun i -> result.[i] <- doWork opts i )
        result


    // Parallel LINQ (using query operators)
    let Example24 opts (source:_[]) = 
        source
          .AsParallel()
          .AsOrdered()
          .Select(doWork opts)
          .ToArray()


    // Parallel LINQ (using F# PSeq module)
    let Example25 opts (source:_[]) = 
        source
          |> PSeq.ordered
          |> PSeq.map (doWork opts)
          |> PSeq.toArray


    // Partitioner with load balancing
    let Example27 opts (source:_[]) = 
        let result = Array.zeroCreate source.Length
        Parallel.ForEach(Partitioner.Create(source, true), fun i ->
            result.[i] <- doWork opts i ) |> ignore
        result

    // --------------------------------------------------------------------------
    // Custom Iterator Example
    // --------------------------------------------------------------------------
        
    // Open task-local state for Parallel.For iteration
    let Example40 opts = 
        let result = Array.zeroCreate opts.NumberOfSteps
        Parallel.For
          ( 0, opts.NumberOfSteps, new ParallelOptions(),
            (fun () -> new Random()),
            (fun i loopState (random:Random) ->
               result.[i] <- random.NextDouble()
               random ),
            ignore ) |> ignore
        result


    // Open task-local state for iteration, with partitioner
    let Example41 opts =
        let result = Array.zeroCreate opts.NumberOfSteps
        Parallel.ForEach
          ( Partitioner.Create(0, opts.NumberOfSteps),
            new ParallelOptions(),
            (fun () -> new Random()),
            (fun (min, max) loopState (random:Random) ->
               for i in min .. max - 1 do
                   result.[i] <- random.NextDouble()
               random),
            ignore ) |> ignore
        result



// ------------------------------------------------------------------------------
// Type that wraps examples for use in the main program ('Program.fs')
// ------------------------------------------------------------------------------

type ParallelForExample(?complexity, ?steps, ?verify, ?simulateError) =

    // Initialize configuration for running examples
    let opts = 
      { LoopBodyComplexity = defaultArg complexity 50
        NumberOfSteps = defaultArg steps 10000000
        VerifyResult = defaultArg verify true
        SimulateInternalError = defaultArg simulateError false }


    /// Runs a parallel computing test that generates float[] as the result. 
    /// Optionally verifies the correctness of the result and handles exceptions.
    let runParallelForExample action label =
        // Clean up from previous run
        GC.Collect()
        try
            let results = SampleUtilities.TimedAction ("  " + label) (fun () -> action opts)
            if opts.VerifyResult then results |> Array.iteri (ensureResult opts)
        with 
          | :? AggregateException as ae ->
              ae.Handle (fun e ->
                  Console.WriteLine("  {0}: Failed with {1}", label, e.GetType().ToString())
                  true)
          | :? ParallelForExampleException as ex ->
              Console.WriteLine("  {0}: Failed with unaggregated {1}  ", label, ex.GetType().ToString())
          | ex ->
              Console.WriteLine("  {0}: Failed with unaggregated {1}  ", label, ex.GetType().ToString())
  

    /// Just like the previous function, but calls action with 'int[]' value as an argument
    /// (this one is used for measuring and testing Parallel.ForEach)
    let runParallelForEachExample action label =
        // Clean up from previous run
        GC.Collect()
        try
            let source = [| 0 .. opts.NumberOfSteps - 1 |]
            let results =  SampleUtilities.TimedAction ("  " + label) (fun () -> action opts source)
            if opts.VerifyResult then results |> Array.iteri (ensureResult opts)
        with
          | :? AggregateException as ae ->
              ae.Handle (fun e ->
                  Console.WriteLine("  {0}: Failed with {1}", label, e.GetType().ToString())
                  true)
          | :? ParallelForExampleException as ex ->
              Console.WriteLine("  {0}: Failed with unaggregated {1}  ", label, ex.GetType().ToString())
          | ex ->
              Console.WriteLine("  {0}: Failed with unaggregated {1}  ", label, ex.GetType().ToString())
  

    // --------------------------------------------------------------------------
    // Public members called from main program
    // --------------------------------------------------------------------------

    /// Runs all the available Parallel.For tests and samples
    member x.DoParallelFor() =
        Console.WriteLine
          ( "Parallel For Examples (opts.LoopBodyComplexity={0}, opts.NumberOfSteps={1})",
            opts.LoopBodyComplexity, opts.NumberOfSteps)

        runParallelForExample Chapter2.Example01 "Sequential for"
        runParallelForExample Chapter2.Example02 "Simple Parallel.For"
        runParallelForExample Chapter2.Example03 "PLINQ ForAll"
        runParallelForExample Chapter2.Example04 "PLINQ Query"
        runParallelForExample Chapter2.Example05 "PLINQ F# PSeq"
        runParallelForExample Chapter2.Example06 "Partitioned"
        runParallelForExample Chapter2.Example07 "Partitioned with fixed ranges"
        Console.WriteLine()


    /// Runs all the available Parallel.ForEach tests and samples
    member x.DoParallelForEach() =
        Console.WriteLine
          ( "Parallel ForEach Examples (opts.LoopBodyComplexity={0}, opts.NumberOfSteps={1})",
            opts.LoopBodyComplexity, opts.NumberOfSteps)

        runParallelForEachExample Chapter2.Example21 "Sequential foreach"
        runParallelForEachExample Chapter2.Example22 "Simple Parallel.ForEach"
        runParallelForEachExample Chapter2.Example23 "PLINQ ForAll"
        runParallelForEachExample Chapter2.Example24 "PLINQ Query"
        runParallelForEachExample Chapter2.Example25 "PLINQ F# PSeq"
        runParallelForEachExample Chapter2.Example27 "Partitioned with load balancing"
        Console.WriteLine()

