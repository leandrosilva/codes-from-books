//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops.Main

open System
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops.CustomIteratorExample
open Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops.Examples

let mainOnThreadpoolThread() =
    Console.WriteLine("Basic Parallel Loops Samples\n");
#if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n");
    let verify = true
#else   
    let verify = false
#endif

    CustomIteratorExample.Example()

    let examples = 
      [| ParallelForExample(complexity = 10000000, steps = 10, verify = verify)
         ParallelForExample(complexity = 1000000, steps = 100, verify = verify)
         ParallelForExample(complexity = 10000, steps = 10000, verify = verify)
         ParallelForExample(complexity = 100, steps = 1000000, verify = verify)
         ParallelForExample(complexity = 10, steps = 10000000, verify = verify) |]

    for e1 in examples do e1.DoParallelFor()
    for e2 in examples do e2.DoParallelForEach()

    Console.WriteLine("\nRun complete... press enter to finish.")
    Console.ReadLine()

// ------------------------------------------------------------------------------
// Run the main test function in thread pool
// ------------------------------------------------------------------------------

do
    Task.Factory.StartNew(mainOnThreadpoolThread).Wait()
