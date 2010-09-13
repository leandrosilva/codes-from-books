//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicAggregation.Examples

open System
open System.Linq
open System.Threading.Tasks
open System.Collections.Concurrent

open Microsoft.FSharp.Collections
open Microsoft.Practices.ParallelGuideSamples.Utilities

/// General transformation before calculating aggregate sum
let private normalize (x:float) = x


// ------------------------------------------------------------------------------
// Examples of sequential / parallel aggregation of sequence of values
// ------------------------------------------------------------------------------

/// Simple sequential (imperative) loop using 'mutable' value
let Chapter4Sample01Sequential (sequence:_[]) =
    let mutable sum = 0.0
    for i in 0 .. sequence.Length - 1 do
        sum <- sum + normalize sequence.[i]
    sum


/// Simple sequential loop using recursion (functional)
let Chapter4Sample01SequentialFunctional (sequence:_[]) =
    let rec loop i sum = 
      if i = sequence.Length then sum
      else loop (i + 1) (sum + normalize sequence.[i])
    loop 0 0.0

/// WARNING: BUGGY CODE. Do not copy this method.
/// This version will run *much slower* than the sequential version
let Chapter4Sample01IncorrectParallel (sequence:_[]) =
    let lockObject = new obj()
    let sum = ref 0.0

    // BUG -- Do not use Parallel.For 
    Parallel.For(0, sequence.Length, fun i ->
        // BUG -- Do not use locking inside of a parallel loop for aggregation
        lock lockObject (fun () ->
            // BUG -- Do not use shared variable for parallel aggregation
            sum := !sum + normalize sequence.[i]) ) |> ignore
    !sum


/// Sequential implementation using LINQ
let Chapter4Sample02Linq (sequence:_[]) =
    sequence.Select(normalize).Sum()

/// Parallel implementation using PLINQ (Note AsParallel call)
let Chapter4Sample02Plinq (sequence:_[]) =
    sequence.AsParallel().Select(normalize).Sum()

/// Parallel implementation using F# PSeq module
let Chapter4Sample02PSeq (sequence:_[]) =
    sequence
      |> PSeq.map normalize
      |> PSeq.sum

/// Parallel implementation with custom aggregation (multiplication) using PLINQ
let Chapter4Sample03Plinq (sequence:_[]) =
    sequence
      .AsParallel()
      .Select(normalize) 
      .Aggregate(1.0, fun y1 y2 -> y1 * y2)

/// Parallel implementation with custom aggregation (multiplication) using F# PSeq module
let Chapter4Sample03PSeq (sequence:_[]) =
    sequence
      |> PSeq.map normalize
      |> PSeq.fold (*) 1.0

/// Parallel implementation using 'ForEach' that aggregates partitions of the 
/// sequence in parallel (loop body) and then aggregates results of partitions 
/// (using locks) 
let Chapter4Sample01Parallel (sequence:_[]) =
    let lockObject = new obj()
    let sum = ref 0.0

    // ForEach<TSource, TLocal>(
    //   IEnumerable<TSource> source, 
    //   Func<TLocal> localInit, 
    //   Func<TSource, ParallelLoopState, TLocal, TLocal> body, 
    //   Action<TLocal> localFinally)
    Parallel.ForEach
      ( // 1- The values to be aggregated
        sequence,

        // 2- The local initial partial result
        (fun () -> 0.0),

        // 3- The loop body
        (fun x loopState partialResult ->
            normalize(x) + partialResult),

        // 4- The final step of each local context            
        (fun localPartialSum ->
            // Enforce serial access to single, shared result
            lock lockObject (fun () -> sum := !sum + localPartialSum)) ) |> ignore
    !sum


/// Parallel implementation using 'ForEach' that aggregates partitions created 
/// explicitly using partitioner and then aggregates results of partitions 
/// (using locks) 
let Chapter4Sample01ParallelPartitions (sequence:_[]) =
    let lockObject = new obj()
    let sum = ref 0.0
    let rangePartitioner = Partitioner.Create(0, sequence.Length)

    // ForEach<TSource, TLocal>(
    //   Partitioner<TSource> source, 
    //   Func<TLocal> localInit, 
    //   Func<TSource, ParallelLoopState, TLocal, TLocal> body, 
    //   Action<TLocal> localFinally)
    Parallel.ForEach
      ( // 1- the input intervals
        rangePartitioner,

        // 2- The local initial partial result
        (fun () -> 0.0),

        // 3- The loop body for each interval
        (fun (rfrom, rto) loopState initialValue -> 
            // imperative aggregation of the partition
            let mutable partialSum = initialValue
            for i in rfrom .. rto - 1 do
                partialSum <- partialSum + (normalize sequence.[i])
            partialSum ),

        // 4- The final step of each local context
        (fun localPartialSum ->
            // Use lock to enforce serial access to shared result
            lock lockObject (fun () -> sum := !sum + localPartialSum)) ) |> ignore
    !sum