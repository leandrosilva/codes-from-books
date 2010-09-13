//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicDynamicTasks.Examples

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.Utilities

type Tree<'T> =
    | Node of Tree<'T> * 'T * Tree<'T>
    | Leaf

// ------------------------------------------------------------------------------
// Parallel processing utilities
// ------------------------------------------------------------------------------

let private parallelWhileNotEmpty initialValues body =
    let mutable from = new ConcurrentQueue<_>(initialValues)
    while not from.IsEmpty do
        let target = new ConcurrentQueue<_>()
        Parallel.ForEach(from, fun v -> body v target.Enqueue) |> ignore
        from <- target

let private parallelWhileNotEmpty2 initialValues body =
    let items = new ConcurrentBag<_>(initialValues)
    let taskList = new ResizeArray<_>()
    let maxTasks = Environment.ProcessorCount * 10
    let mutable taskCount = 0
    let mutable stop = false
    while not stop do
        let tasks = taskList.ToArray()
        if tasks.Length > 0 then
            Task.WaitAll(tasks)
            taskList.Clear()
            taskCount <- 0
        if items.IsEmpty then stop <- true
        else
            let mutable item = Unchecked.defaultof<_>
            while taskCount < maxTasks && items.TryTake(&item) do
                let v = item
                let task = Task.Factory.StartNew(fun () -> body v items.Add)
                taskList.Add(task)
                taskCount <- taskCount + 1

// ------------------------------------------------------------------------------
// Various utilities for working with and traversing trees
// ------------------------------------------------------------------------------

/// Generate tree with the specified number of nodes
/// and with the specified density of branches
let makeTree nodeCount density =
    if nodeCount < 1 then invalidArg "nodeCount" "out of range"
    if not (0.0 < density && density <= 1.0) then invalidArg "density" "out of range"

    let rec makeTreeUtil nodeCount density offset (r:Random) =
        let flip1 = r.NextDouble() > density
        let flip2 = r.NextDouble() > density
        let newCount = nodeCount - 1
        let count1, count2 = 
            let c = if flip1 && flip2 then newCount / 2 elif flip1 then newCount else 0
            if r.NextDouble() > 0.5 then c, newCount - c else newCount - c, c
        let l = if count1 > 0 then makeTreeUtil count1 density (offset + 1) r else Leaf
        let r = if count2 > 0 then makeTreeUtil count2 density (offset + 1 + count1) r else Leaf
        Node(l, offset.ToString(), r)

    makeTreeUtil nodeCount density 0 (new Random())


/// Iterate over all nodes of the tree and call the provided action 
/// for every node (sequential non-tail recursive implementation)
let rec private sequentialWalk action tree = 
    match tree with 
    | Node(l, d, r) -> 
        action d
        sequentialWalk action l
        sequentialWalk action r
    | _ -> ()


/// Iterate over all nodes in parallel (using Tasks)
let rec private parallelWalk action tree = 
    match tree with 
    | Node(l, d, r) ->
        let t1 = Task.Factory.StartNew(fun () -> 
            action(d))
        let t2 = Task.Factory.StartNew(fun () -> 
            parallelWalk action l)
        let t3 = Task.Factory.StartNew(fun () -> 
            parallelWalk action r)
        Task.WaitAll(t1, t2, t3)
    | _ -> () 

/// Iterates over all nodes in parallel (using Tasks)
/// Created tasks are attached to parents uing the 'AttachedToParent' option
let rec private parallelWalkAttached action tree = 
    match tree with 
    | Node(l, d, r) ->
        let t1 = Task.Factory.StartNew((fun () -> 
            action(d)), TaskCreationOptions.AttachedToParent)
        let t2 = Task.Factory.StartNew((fun () -> 
            parallelWalkAttached action l), TaskCreationOptions.AttachedToParent)
        let t3 = Task.Factory.StartNew((fun () -> 
            parallelWalkAttached action r), TaskCreationOptions.AttachedToParent)
        Task.WaitAll(t1, t2, t3)
    | _ -> () 


/// Iterates over all nodes in parallel (using Tasks)
/// Uses queue (hidden in 'ParallelWhileNotEmpty' to keep track of the remaining work)
let private parallelWalkWithQueue action tree = 
    parallelWhileNotEmpty [ tree ] (fun tree adder ->
      match tree with 
      | Node(l, d, r) ->
          if l <> Leaf then adder l
          if r <> Leaf then adder r 
          action d
      | _ -> () )

/// Iterates over all nodes in parallel (using Tasks)
/// Uses queue (hidden in 'ParallelWhileNotEmpty2' to keep track of the remaining work)
let private parallelWalkWithQueue2 action tree = 
    parallelWhileNotEmpty2 [ tree ] (fun tree adder ->
      match tree with 
      | Node(l, d, r) ->
          if l <> Leaf then adder l
          if r <> Leaf then adder r 
          action d
      | _ -> () )

/// Iterate over all nodes in parallel (using F# asynchronous workflows)
/// Recursive implementation that starts processing as a child async workflow
let rec private parallelWalkAsync action tree = async {
    match tree with 
    | Node(l, d, r) ->
        // Start recursive processing and process current element
        let! t1 = Async.StartChild(parallelWalkAsync action l)
        let! t2 = Async.StartChild(parallelWalkAsync action r)
        do! action d
        // wait for completion of recursive processing & return
        let! r1 = t1
        let! r2 = t2 
        return () 
    | _ -> return () }

// ------------------------------------------------------------------------------
// Examples from the chapter 6 
// ------------------------------------------------------------------------------

module Chapter6 = 

    let Example1Sequential n time tree = 
        for i in 0 .. n - 1 do
            let result = new ResizeArray<_>()
            tree |> sequentialWalk (fun data ->
                SampleUtilities.DoCpuIntensiveOperationSimple time |> ignore
                result.Add(data) )
        Console.WriteLine()

    let Example1Parallel n time tree = 
        for i in 0 .. n - 1 do
            let result = new ConcurrentBag<_>()
            tree |> parallelWalk (fun data ->
                SampleUtilities.DoCpuIntensiveOperationSimple time |> ignore
                result.Add(data) )

    let Example1ParallelAsync n time tree = 
        for i in 0 .. n - 1 do
            let result = new ConcurrentBag<_>()
            tree |> parallelWalkAsync (fun data -> 
              // asynchronous workflow that could include non-blocking I/O 
              async {
                SampleUtilities.DoCpuIntensiveOperationSimple time |> ignore 
                result.Add(data) }) |> Async.RunSynchronously

    let Example1ParallelAttached n time tree =
        for i in 0 .. n - 1 do
            let result = new ConcurrentBag<_>()
            tree |> parallelWalkAttached (fun data ->
                SampleUtilities.DoCpuIntensiveOperationSimple time |> ignore
                result.Add(data) )

    let Example01ParallelWhileNotEmpty n time tree = 
        for i in 0 .. n - 1 do
            let result = new ConcurrentBag<_>()
            tree |> parallelWalkWithQueue (fun data ->
                SampleUtilities.DoCpuIntensiveOperationSimple time |> ignore
                result.Add(data) )

    let Example01ParallelWhileNotEmpty2 n time tree = 
        for i in 0 .. n - 1 do
            let result = new ConcurrentBag<_>()
            tree |> parallelWalkWithQueue2 (fun data ->
                SampleUtilities.DoCpuIntensiveOperationSimple time |> ignore
                result.Add(data) )