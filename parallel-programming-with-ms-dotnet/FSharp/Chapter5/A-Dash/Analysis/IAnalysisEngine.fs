//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash

open System
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects

/// Represents an engine that performs the analysis
/// (analysis can be performed sequentially, in parallel using tasks
/// and also in parallel using non-blocking I/O using F# asyncs)
[<AllowNullLiteral>]
type IAnalysisEngine =
    inherit IDisposable

    // Starts analysis in background and returns type that can be 
    // used to register notifications when result becomes available
    abstract DoAnalysisParallel : unit -> AnalysisNotifications
    abstract DoAnalysisAsync : unit -> AnalysisNotifications
    
    // Runs analysis and returns calculated results
    abstract DoAnalysisSequential : unit -> MarketRecommendation

    abstract TryCancelAnalysis : unit -> unit
