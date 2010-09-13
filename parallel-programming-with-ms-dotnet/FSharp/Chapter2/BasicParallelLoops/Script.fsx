//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

#r @"..\..\Utilities\bin\Release\Utilities.dll"
#load @"..\..\PSeq.fs"

#load "ParallelForExample.fs"
#load "CustomIteratorExample.fs"

open System
open Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops.Examples
open Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops.CustomIteratorExample

// *** TODO ***
// You can chose one of the sample configurations here

/// Configuration of the sample run
let opts = { 
    // LoopBodyComplexity = 10000000; NumberOfSteps = 10
    // LoopBodyComplexity = 1000000; NumberOfSteps = 100
    // LoopBodyComplexity = 100000; NumberOfSteps = 1000
    LoopBodyComplexity = 10000; NumberOfSteps = 10000
    // LoopBodyComplexity = 1000; NumberOfSteps = 100000
    // LoopBodyComplexity = 100; NumberOfSteps = 1000000
    // LoopBodyComplexity = 10; NumberOfSteps = 10000000
    VerifyResult = false; SimulateInternalError = false }


// ------------------------------------------------------------------------------
// Examples of using for loop, paralellel for loop, or Enumerable.Range in PLINQ
// ------------------------------------------------------------------------------

// Run example demonstrating 'ForEach' with custom iterator
CustomIteratorExample.Example()


// Clean up from previous run
GC.Collect()

// Turn on the timing in F# Interactive
#time "on"

do
    // *** TODO ***: 
    // You can run one of the samples here

    let results = Chapter2.Example01 opts // Sequential for
    let results = Chapter2.Example02 opts // Simple Parallel.For
    let results = Chapter2.Example03 opts // PLINQ ForAll
    let results = Chapter2.Example04 opts // PLINQ Query
    let results = Chapter2.Example04 opts // PLINQ F# PSeq
    let results = Chapter2.Example06 opts // Partitioned
    let results = Chapter2.Example07 opts // Partitioned with fixed ranges

    // Verify that the result is correct (throws an exception if error is found)
    results |> Array.iteri (ensureResult opts)
  
#time "off"

// ------------------------------------------------------------------------------
// Examples of using foreach loop, paralellel foreach loop, or PLINQ
// ------------------------------------------------------------------------------

// Generate source data for 'foreach' samples
let source = [| 0 .. opts.NumberOfSteps - 1 |]

// Clean up from previous run
GC.Collect()

// Turn on the timing in F# Interactive
#time "on"

do
    // *** TODO ***: 
    // You can run one of the samples here

    let results = Chapter2.Example21 opts source // Sequential foreach
    let results = Chapter2.Example22 opts source // Simple Parallel.ForEach
    let results = Chapter2.Example23 opts source // PLINQ ForAll
    let results = Chapter2.Example24 opts source // PLINQ Query
    let results = Chapter2.Example25 opts source // PLINQ F# PSeq
    let results = Chapter2.Example27 opts source // Partitioned with load balancing

    // Verify that the result is correct (throws an exception if error is found)
    results |> Array.iteri (ensureResult opts)

#time "off"