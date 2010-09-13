//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicParallelTasks.Main

open System
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.BasicParallelTasks.Examples


// ------------------------------------------------------------------------------
// Main - run all the tests
// ------------------------------------------------------------------------------

let main() =
    Console.WriteLine("Basic Parallel Tasks Samples\n")
#if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n")
#endif
    SampleUtilities.TimedAction "2 steps, sequential" Chapter3Sample01Sequential
    SampleUtilities.TimedAction "2 steps (Task.Wait), parallel" Chapter3Sample01ParallelTask
    SampleUtilities.TimedAction "2 steps, parallel invoke" Chapter3Sample01ParallelInvoke

    SampleUtilities.TimedAction "Speculative Execution" Chapter3Sample03
    SampleUtilities.TimedAction "Task.WaitAny" Chapter3Sample04

    ExampleOfIncorrectClosure()
    ExampleOfCorrectClosure()
    ExampleOfIncorrectDispose()
    ExampleOfCorrectDispose()

    Console.WriteLine("\nRun complete... press enter to finish.")
    Console.ReadLine()

// ------------------------------------------------------------------------------
// Run the main testing program in the thread pool

do
    Task.Factory.StartNew(main).Wait()
