//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicFutures.Main

open System
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.BasicFutures

// Note: for consistent timing results, run these without the debugger. 
// Observe CPU usage using the task manager. On a multicore machine, the sequential 
// version will use less CPU and execute more slowly than the parallel versions.

do
    Console.WriteLine("Basic Futures Samples\n")
#if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n")
#endif
    Console.WriteLine("Starting...")

    // timed comparison between sequential and two ways of using the futures pattern
    SampleUtilities.TimedRun "Sequential" Examples.Example1 |> ignore
    SampleUtilities.TimedRun "Parallel, using F1 future" Examples.Example2 |> ignore
    SampleUtilities.TimedRun "Parallel, using F2/F3 future" Examples.Example3 |> ignore

    // additional variants for comparison
    Console.WriteLine()
    Console.WriteLine("Other variants, for comparison--")
    SampleUtilities.TimedRun "Parallel, using F2 future and F3 continuation" Examples.Example4 |> ignore
    SampleUtilities.TimedRun "Parallel, using F1 and F2/F3 future" Examples.Example5 |> ignore
    SampleUtilities.TimedRun "Parallel, with try/catch block" Examples.Example6 |> ignore

    Console.WriteLine("\nRun complete... press enter to finish.") 
    Console.ReadLine() |> ignore