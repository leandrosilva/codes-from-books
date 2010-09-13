//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicParallelTasks.Examples

open System
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.Utilities
open System.Threading
open System.Linq
open System.IO

let mutable TaskSeconds = 1.0

// ------------------------------------------------------------------------------
// Functions that perform some "work"
// ------------------------------------------------------------------------------

let private doLeft() =
    SampleUtilities.DoCpuIntensiveOperationSimple(TaskSeconds * 0.2) |> ignore

let private doRight() =
    SampleUtilities.DoCpuIntensiveOperationSimple(TaskSeconds * 0.3) |> ignore

let private doCenter() =
    SampleUtilities.DoCpuIntensiveOperationSimple(TaskSeconds * 0.2) |> ignore

let private searchCenter (token:CancellationToken) =
    token.ThrowIfCancellationRequested()
    SampleUtilities.DoCpuIntensiveOperation (TaskSeconds * 0.5) token |> ignore
    token.ThrowIfCancellationRequested()

let private searchLeft (token:CancellationToken) =
    token.ThrowIfCancellationRequested()
    SampleUtilities.DoCpuIntensiveOperation (TaskSeconds * 0.2) token |> ignore
    token.ThrowIfCancellationRequested()

let private searchRight (token:CancellationToken) =
    token.ThrowIfCancellationRequested()
    SampleUtilities.DoCpuIntensiveOperation (TaskSeconds * 0.3) token |> ignore
    token.ThrowIfCancellationRequested()

// ------------------------------------------------------------------------------
// Task examples and demos
// ------------------------------------------------------------------------------

/// Execute two operations sequentially
let Chapter3Sample01Sequential() =
    doLeft()
    doRight()


/// Execute operations in parallel using tasks explicitly
let Chapter3Sample01ParallelTask() =
    let t1 = Task.Factory.StartNew(doLeft)
    let t2 = Task.Factory.StartNew(doRight)
    Task.WaitAll(t1, t2)


/// Execute operations in parallel using Parallel.Invoke
let Chapter3Sample01ParallelInvoke() = 
    Parallel.Invoke(new Action(doLeft), new Action(doRight))


/// Invoke multiple actions in parallel and wait until the 
/// first operation completes (others will be canceled)
let private speculativeInvoke actions =
    let cts = new CancellationTokenSource()
    let token = cts.Token
    let tasks = 
      [| for a in actions ->
             Task.Factory.StartNew((fun () -> a(token)), token) :> Task |]
    Task.WaitAny(tasks) |> ignore
    cts.Cancel()
    try
        try Task.WaitAll(tasks) 
        with 
        | :? AggregateException as ae ->
              ae.Flatten().Handle(fun e -> e :? OperationCanceledException)
    finally
        if cts <> null then cts.Dispose()


/// Execute operations speculatively in parallel
let Chapter3Sample03() = 
    speculativeInvoke [| searchLeft; searchRight; searchCenter |]


/// Waits for all tasks by waiting at any task repeatedly and
/// removing the completed task from an array of tasks
let Chapter3Sample04() =
    let alltasks = 
          [| Task.Factory.StartNew(doLeft)
             Task.Factory.StartNew(doRight)
             Task.Factory.StartNew(doCenter) |]
    
    let rec loop tasks = 
        let taskIndex = Task.WaitAny(tasks)
        Console.WriteLine("Finished task {0}.", taskIndex + 1)
        let tasks = tasks |> Array.filter (fun t -> t <> tasks.[taskIndex])
        if tasks.Length > 0 then loop tasks

    loop alltasks
      
    try Task.WaitAll(alltasks)
    with :? AggregateException as ae ->
        ae.Handle (fun e ->
            if (e :? InvalidOperationException) then
                Console.WriteLine("Saw expected exception.")
                true
            else false )

// ------------------------------------------------------------------------------
// Demonstration of (in)correct uses of closures
// ------------------------------------------------------------------------------

// This is really only problem for C#/VB.Net, because F# treats loop variables
// differently (each iteration uses a new value). However, we can demonstrate
// the same thing more explicitly using mutable values


/// If we use a reference cell (simple 'mutable' values cannot be captured)
/// then the same reference cell is shared by multiple tasks (this is what
/// happens in C# when using the index variable directly)
let ExampleOfIncorrectClosure() =
    Console.WriteLine("Incorrectly written closure returns unexpected values:")
    let tasks = Array.zeroCreate 4
    let i = ref 0 
    while !i < 4 do
        tasks.[!i] <- Task.Factory.StartNew(fun () -> 
            Console.WriteLine(!i))
        i := !i + 1
    Task.WaitAll(tasks)


/// In F#, we can safely capture the index variable
let ExampleOfCorrectClosure() = 
    Console.WriteLine("Correctly written closure returns expected values:")
    let tasks = Array.zeroCreate 4
    for i in 0 .. 3 do
        tasks.[i] <- Task.Factory.StartNew(fun () -> 
            Console.WriteLine(i))
    Task.WaitAll(tasks)


/// When we dispose of a resource using 'use', we cannot use it from a task
/// (that captures it) after it has been disposed. The following will cause 
/// an ObjectDisposedException:
let ExampleOfIncorrectDispose() = 
    try
        let t =
          use file = new StringReader("text")
          Task.Factory.StartNew(fun () -> file.ReadLine())
        // WARNING: BUGGY CODE, file has been disposed
        Console.WriteLine("File has been disposed now")
        Console.WriteLine(t.Result)
    with :? AggregateException as ae ->
        ae.Handle(fun e ->
          match e with 
          | :? ObjectDisposedException ->
              Console.WriteLine("Saw expected error: {0}", e.Message)
              true
          | _ -> false )


/// If the body of the task evaluates before the main thread leaves 
/// the scope in which the resource is disposed of using 'use' 
/// then everything works correctly:
let ExampleOfCorrectDispose() =
    ( use file = new StringReader("text")
      let t = Task.Factory.StartNew(fun () -> file.ReadLine())
      Console.WriteLine("Saw correct output: {0}", t.Result) )
    Console.WriteLine("File has been disposed now")
