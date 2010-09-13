//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.RelatedPatterns

open System
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.Utilities

// Future based programming model

type IWithFutures =
    abstract Start : unit -> Task<int>

type FuturesBased() =
    interface IWithFutures with
        member x.Start() =
            Task.Factory.StartNew(fun () ->
                SampleUtilities.DoCpuIntensiveOperation(2.0) |> ignore
                42 )

// Event based programming model (adapter for the future based model)

type CompletedEventArgs(result:int) = 
    inherit EventArgs()
    member x.Result = result

type IWithEvents =
    abstract Start : unit -> unit
    abstract Completed : IEvent<CompletedEventArgs>
 
type EventBased() =
    let instance = new FuturesBased() :> IWithFutures
    let completedEvt = new Event<_>()

    interface IWithEvents with
        member x.Completed = completedEvt.Publish
        member x.Start() =
            let task = instance.Start()
            task.ContinueWith(new Action<Task<_>>(fun t ->
                completedEvt.Trigger(new CompletedEventArgs(t.Result)))) |> ignore
