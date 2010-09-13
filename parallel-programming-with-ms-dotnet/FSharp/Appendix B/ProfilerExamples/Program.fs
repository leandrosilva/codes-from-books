//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.ProfilerExamples

open System
open System.Linq
open System.Threading
open System.Threading.Tasks

let deadlock() =
    let obj1 = new obj()
    let obj2 = new obj()
    let rec numbers x = seq { 
        yield x
        yield! numbers (x+1) }

    Parallel.Invoke
      ( new Action(fun () ->
          for i in numbers 0 do
              lock obj1 (fun () ->
                  Console.WriteLine("[1] Got 1 at {0}", i)
                  lock obj2 (fun () -> Console.WriteLine("[1] Got 2 at {0}", i)))),
        new Action(fun () ->
          for i in numbers 0 do
              lock obj2 (fun () ->
                  Console.WriteLine("[2] Got 2 at {0}", i)
                  lock obj1 (fun () -> Console.WriteLine("[2] Got 1 at {0}", i)) ) ))


let lockContention() = 
    let syncObj = new obj()

    for p in 0 .. Environment.ProcessorCount - 1 do
        let thread = new Thread(fun () ->
            for i in 0 .. 49 do
                // Do work
                for j in 0 .. 1000 do ()
                // Do protected work
                lock syncObj (fun () ->
                    for j in 0 .. 100000000 do ()))
        thread.Start()


let oversubscription() =
    for i in 0 .. Environment.ProcessorCount * 4 do
        let thread = new Thread(fun () ->
            // Do work 
            for j in 0 .. 1000000000 do ())
        thread.Start()


let loadImbalance() =
      let loadFactor = 10
      ParallelEnumerable.Range(0, 100000).ForAll(fun i ->
        // Do work
        for j in 0 .. i * loadFactor do ())


let help() =
    Console.WriteLine("Usage: [deadlock|lockcontention|oversubscription|loadimbalance]")

[<EntryPoint>]
let main (args:string[]) =
    Console.WriteLine("Profiler Samples\n")
    Console.WriteLine("Press any key to start run after the profiler has initiliazed...")
    Console.ReadKey() |> ignore
    Console.WriteLine("Starting...\n")
    match args |> Array.map (fun s -> s.Trim().ToLower()) with 
    | [| "deadlock" |] ->
        Console.WriteLine("Showing deadlock.")
        Console.WriteLine("WARNING: This program does not terminate!")
        deadlock()
    | [| "lockcontention" |] ->
        Console.WriteLine("Showing lock contention.")
        lockContention()
    | [| "oversubscription" |] ->
        Console.WriteLine("Showing oversubscription.")
        oversubscription()
    | [| "loadimbalance" |] ->
        Console.WriteLine("Showing load imbalance.")
        loadImbalance()
    | _ -> help()
    Console.WriteLine("\nRun complete...")
    0