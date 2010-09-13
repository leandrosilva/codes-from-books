//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

#load @"..\..\PSeq.fs"
#r @"..\..\Utilities\bin\Release\Utilities.dll"

open System
open System.Linq
open System.Diagnostics
open System.Threading.Tasks
open System.Collections.Concurrent

open Microsoft.FSharp.Collections
open Microsoft.Practices.ParallelGuideSamples.Utilities

// ------------------------------------------------------------------------------
// Working with histograms
// ------------------------------------------------------------------------------

let tableSize = 40
let bucketSize = 5

let makeEmptyHistogram() : int[] =
    Array.zeroCreate tableSize

let combineHistograms histogram1 histogram2 =
    Array.map2 (+) histogram1 histogram2 

let printHistogram histogram = 
    histogram |> Array.iteri (fun j v ->
        Console.WriteLine("{0} {1}", (int)(j * bucketSize), v))


// ------------------------------------------------------------------------------
// Sequential and parallel simulation example
// ------------------------------------------------------------------------------
            
/// Placeholder for a user-written simulation routine. For example, this 
/// could be a financial simulation that explores various risk outcomes.
/// This placeholder just transforms the value so that the outputs of
/// simulation will follow a bell curve.
let doSimulation sampleValue mean stdDev =
    SampleUtilities.GaussianInverse sampleValue mean stdDev


/// Runs simulation for several randomly generated inputs and 
/// stores the result in a bucket of a histogram (sequentially)
let doSequentialAggregation count mean stdDev =
    let generator = new Random(SampleUtilities.MakeRandomSeed())
    let histogram = makeEmptyHistogram()

    for i in 0 .. count - 1 do 
        // get the next input value
        let sample = generator.NextDouble()
        if sample > 0.0 then
            // MAP: perform a simulation trial for the sample value
            let simulationResult = doSimulation sample mean stdDev
            
            // REDUCE: merge the result of simulation into a histogram
            let histogramBucket = int (Math.Floor(simulationResult / float bucketSize))
            if 0 <= histogramBucket && histogramBucket < tableSize then
                histogram.[histogramBucket] <- histogram.[histogramBucket] + 1
    histogram


/// Uses overload of 'Parallel.ForEach' that keeps separate state for each thread.
/// Performs the above on every thread and then aggregates all results
let doParallelAggregation count mean stdDev =
    // Partition the iterations
    let rangePartitioner = Partitioner.Create(0, count)
    let histogram = makeEmptyHistogram()

    // ForEach<TSource, TLocal>(Partitioner<TSource> source, 
    //                          Func<TLocal> localInit, 
    //                          Func<TSource, ParallelLoopState, TLocal, TLocal> body, 
    //                          Action<TLocal> localFinally)
    Parallel.ForEach
      ( // 1- the partitioner object
        rangePartitioner,

        // 2- the local state object that will 
        // collect results for a single partition
        (fun () -> makeEmptyHistogram()),

        // 3- the body that will be invoked once for each of 
        // the partitions created by the partitioner. (The number of
        // partitions depends on the number of cores.)
        (fun (rfrom, rto) loopState index (localHistogram:int[]) -> 

            // Make a local generator to avoid conflicts between partitions.
            let generator = new Random(SampleUtilities.MakeRandomSeed())

            // Iterate over the range assigned by the partitioner
            for i in rfrom .. rto - 1 do
                // With each iteration get the next input value 
                let sample = generator.NextDouble()
                if sample > 0.0 then
                    // MAP: perform a simulation trial for the sample value
                    let simulationResult = doSimulation sample mean stdDev

                    // REDUCE: merge the result of simulation into the local histogram
                    let histogramBucket = int (Math.Floor(simulationResult / float bucketSize))
                    if 0 <= histogramBucket && histogramBucket < tableSize then
                        localHistogram.[histogramBucket] <- localHistogram.[histogramBucket] + 1
            localHistogram ),

        // 4- The finalizer that will be run once for each partition to combine
        // results created for each partition.
        (fun (localHistogram:int[]) ->
            // Use lock to enforce serial access to single, shared result
            lock histogram (fun () ->
                // MERGE: local histogram results are added into the global histogram
                for i in 0 .. tableSize - 1 do 
                    histogram.[i] <- histogram.[i] + localHistogram.[i])) ) |> ignore

    // Return the global histogram
    histogram


/// Implements the above algorithm using 'Aggregate' method from Parallel LINQ
let doParallelAggregationPlinq count mean stdDev =
    // Aggregate<TSource, TAccumulate, TResult>(
    //   this ParallelQuery<TSource> source, 
    //   Func<TAccumulate> seedFactory, 
    //   Func<TAccumulate, TSource, TAccumulate> updateAccumulatorFunc, 
    //   Func<TAccumulate, TAccumulate, TAccumulate> combineAccumulatorsFunc, 
    //   Func<TAccumulate, TResult> resultSelector)        
    ParallelEnumerable.Range(0, count).Aggregate<int, int[] * Random, int[]>
      ( 
        // 1- create an empty local accumulator object
        //    that includes all task-local state
        (fun () -> makeEmptyHistogram(), new Random(SampleUtilities.MakeRandomSeed())),

        // 2- run the simulation, adding result to local accumulator 
        (fun (histogram:int[], rnd:Random) i ->
            // With each iteration get the next random value 
            let sample = rnd.NextDouble()
            if sample > 0.0 && sample < 1.0 then

                // Perform a simulation trial for the sample value
                let simulationResult = doSimulation sample mean stdDev

                // Put the result of simulation into the histogram of the local accumulator
                let histogramBucket = int (Math.Floor(simulationResult / float bucketSize))
                if 0 <= histogramBucket && histogramBucket < tableSize then
                    histogram.[histogramBucket] <- histogram.[histogramBucket] + 1
            (histogram, rnd)),
        
        // 3- Combine local results pairwise.
        (fun (histogram1, _) (histogram2, _) ->
            combineHistograms histogram1 histogram2, null), 
        
        // 4- Extract answer from final combination
        fst)


/// Implements the above algorithm using 'mapReduce' function declared above (F# specific version)
/// (PSeq.mapReduce is just a thin wrapper over the 'Aggregate' method from PLINQ)
let doParallelAggregationPSeq count mean stdDev =
    [ 0 .. count ] 
      |> PSeq.ofList
      |> PSeq.mapReduce
        // 1- create an empty local accumulator object
        //    that includes all task-local state
        (fun () -> makeEmptyHistogram(), new Random(SampleUtilities.MakeRandomSeed()))

        // 2- run the simulation, adding result to local accumulator 
        (fun (histogram:int[], rnd:Random) i ->
            // With each iteration get the next random value 
            let sample = rnd.NextDouble()
            if sample > 0.0 && sample < 1.0 then

                // Perform a simulation trial for the sample value
                let simulationResult = doSimulation sample mean stdDev

                // Put the result of simulation into the histogram of the local accumulator
                let histogramBucket = int (Math.Floor(simulationResult / float bucketSize))
                if 0 <= histogramBucket && histogramBucket < tableSize then
                    histogram.[histogramBucket] <- histogram.[histogramBucket] + 1
            (histogram, rnd))
        
        // 3- Combine local results pairwise.
        (fun (histogram1, _) (histogram2, _) ->
            combineHistograms histogram1 histogram2, null)
        
        // 4- Extract answer from final combination
        fst

// ------------------------------------------------------------------------------
// Main run the aggregation example
// ------------------------------------------------------------------------------

let trialCount = 5000000
let mean = 102.5
let stdDev = 15.0

// Turn on timing in the F# Interactive 
#time "on"

// Sequential version
let histogram1 = doSequentialAggregation trialCount mean stdDev
printHistogram(histogram1)

// Parallel version using Parallel.ForEach
let histogram2 = doParallelAggregation trialCount mean stdDev
printHistogram(histogram2)

// Parallel version using PLINQ
let histogram3 = doParallelAggregationPlinq trialCount mean stdDev
printHistogram(histogram3)

#time "off"