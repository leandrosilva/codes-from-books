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
#load "Examples.fs"

open System
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.BasicAggregation


/// Specify the length of the sample sequence to be processed here
let sequence = SampleUtilities.Range(100000000)

#time "on"

// Collect garbage before running example calculations to get more precise timing
GC.Collect()

// Calculate sum, sequential for loop
Examples.Chapter4Sample01Sequential(sequence)
// Calculate sum, sequential, functional using recursion
Examples.Chapter4Sample01SequentialFunctional(sequence)

// Calculate sum, incorrectly coded parallel loop
Examples.Chapter4Sample01IncorrectParallel(sequence)
// Calculate sum, parallel for each
Examples.Chapter4Sample01Parallel(sequence)

// Calculate sum, LINQ (sequential)
Examples.Chapter4Sample02Linq(sequence)
// Calculate sum, PLINQ (parallel)
Examples.Chapter4Sample02Plinq(sequence)
// Calculate sum, F# PSeq (parallel)
Examples.Chapter4Sample02PSeq(sequence)

// Custom aggregation (product) PLINQ (parallel)
Examples.Chapter4Sample03Plinq(sequence)
// Custom aggregation (product) F# PSeq (parallel)
Examples.Chapter4Sample03PSeq(sequence)

// Calculate sum, parallel partitions
Examples.Chapter4Sample01ParallelPartitions(sequence)

#time "off"