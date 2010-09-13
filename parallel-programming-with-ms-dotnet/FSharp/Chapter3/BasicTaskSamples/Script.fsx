//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

#r @"..\..\Utilities\bin\Release\Utilities.dll"
#load "Examples.fs"

open System
open Microsoft.Practices.ParallelGuideSamples.BasicParallelTasks

// ------------------------------------------------------------------------------
// Examples of parallel processing performance
// ------------------------------------------------------------------------------

// Mutable field that specifies the time needed to run 
// a primitive CPU intensive operation used in the tasks
Examples.TaskSeconds <- 1.0

// Turn on the timing in F# Interactive
#time "on"

// 2 steps, sequential
Examples.Chapter3Sample01Sequential()
// 2 steps (Task.Wait), parallel
Examples.Chapter3Sample01ParallelTask()
// 2 steps, parallel invoke
Examples.Chapter3Sample01ParallelInvoke()

// Speculative Execution
Examples.Chapter3Sample03()
// Task.WaitAny
Examples.Chapter3Sample04()

#time "off"

// ------------------------------------------------------------------------------
// Examples of correct and incorrect uses of various features
// ------------------------------------------------------------------------------

Examples.ExampleOfIncorrectClosure()
Examples.ExampleOfCorrectClosure()
Examples.ExampleOfIncorrectDispose()
Examples.ExampleOfCorrectDispose()
