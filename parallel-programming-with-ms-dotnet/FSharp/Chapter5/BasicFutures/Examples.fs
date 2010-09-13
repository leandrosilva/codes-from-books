//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

/// This program shows the simplest use case for the futures 
/// and continuations pattern. Refer to Chapter 5 of the text.
module Microsoft.Practices.ParallelGuideSamples.BasicFutures.Examples

open System
open System.Threading.Tasks
open System.Windows.Controls
open Microsoft.Practices.ParallelGuideSamples.Utilities
open System.Diagnostics.CodeAnalysis

// ------------------------------------------------------------------------------
// Several computationally intensive functions
// ------------------------------------------------------------------------------

let private f1 value = 
    SampleUtilities.DoCpuIntensiveOperationSimple 2.0 |> ignore
    value * value

let private f2 value = 
    SampleUtilities.DoCpuIntensiveOperationSimple 1.0 |> ignore
    value - 2

let private f3 value = 
    SampleUtilities.DoCpuIntensiveOperationSimple 1.0 |> ignore
    value + 1

let private f4 value1 value2 = 
    SampleUtilities.DoCpuIntensiveOperationSimple 0.1 |> ignore
    value1 + value2

// ------------------------------------------------------------------------------
// Sequential and parallel implementation of sample problem
// ------------------------------------------------------------------------------

/// Sequential example
let Example1() = 
    let a = 22

    let b = f1 a 
    let c = f2 a 
    let d = f3 c 
    let f = f4 b d 
    f

/// A parallel example that uses the futures pattern for f1
let Example2() =
    let a = 22

    let bf = Task.Factory.StartNew(fun () -> f1 a)
    let c = f2 a
    let d = f3 c
    let f = f4 bf.Result d
    f

/// A parallel example that uses the futures pattern for f2/f3
let Example3() = 
    let a = 22

    let df = Task.Factory.StartNew(fun () -> f3 (f2 a))
    let b = f1 a
    let f = f4 b df.Result
    f

/// A parallel example that uses the futures and continations pattern.
/// (this is to illustrate syntax only there is no performance 
/// benefit in this case over Example 2 or 3 above)
let Example4() =
    let a = 22

    let cf = Task.Factory.StartNew(fun () -> f2 a)
    let df = cf.ContinueWith(fun (t:Task<_>) -> f3 t.Result)
    let b = f1 a
    let f = f4 b df.Result
    f

/// A parallel example that uses the futures pattern applied to two values.
/// (this is for comparison only there is no performance benefit in this 
/// case over Example 2 or 3 above. You should pattern your own code after 
/// either Example 2 or 3, not this method)
let Example5() =
    let a = 22
    let bf = Task.Factory.StartNew(fun () -> f1 a)
    let df = Task.Factory.StartNew(fun () -> f3(f2 a))
    let f = f4 bf.Result df.Result
    f

/// Futures implemented by the Future<T> type defer exceptions until the 
/// Result property is read, so if an error occurs in the body of 'futureD'
/// it will be handled in the try-finally block
[<SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes")>]
let Example6() =
    let a = 22

    let futureD = Task.Factory.StartNew(fun () -> f3 (f2 a))
    try
        let b = f1 a
        let f = f4 b futureD.Result
        f
    with _ -> // any exception
        Console.WriteLine("Saw exception")
        -1

/// A parallel example that displays the result in a texbox using WPF dispatcher
let Example7 (myTextBox:TextBox) =
    let a = 22 
            
    let futureB = Task.Factory.StartNew(fun () -> f1 a)
    let futureD = Task.Factory.StartNew(fun () -> f3 (f2 a))

    let futureF = Task.Factory.ContinueWhenAll
                    ([| futureB; futureD |],
                     fun tasks -> f4 futureB.Result futureD.Result)
  
    futureF.ContinueWith(fun (t:Task<_>) ->
      myTextBox.Dispatcher.Invoke
        (new Action(fun () -> myTextBox.Text <- t.Result.ToString() )) ) |> ignore


/// A demonstration showing how to construct a task that starts a 
/// delegate asynchronously and waits for its completion
let Example8() = 
    let cb = new AsyncCallback(fun _ -> ())
    let a = new Action(fun () -> Console.WriteLine("Hello"))
    let t1 = Task.Factory.FromAsync(a.BeginInvoke(cb, null), a.EndInvoke)
    t1.Wait()
