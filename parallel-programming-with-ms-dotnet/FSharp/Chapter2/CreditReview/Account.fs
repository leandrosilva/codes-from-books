//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.CreditReview

open System
open Microsoft.Practices.ParallelGuideSamples.Utilities

/// One customer's account data: array of monthly balances, also predictions and warnings
type Account(months, overdraft:float) = 
    // Constructor, allocate balance history for months, assign overdraft
    let balance = Array.zeroCreate months

    let mutable seqPred = 0.0
    let mutable parPred = 0.0
    let mutable plinqPred = 0.0
    let mutable pseqPred = 0.0

    let mutable seqWarn = false
    let mutable parWarn = false
    let mutable plinqWarn = false
    let mutable pseqWarn = false

    static let rateScale = 100.0
    static let balanceScale = 100.0

    member x.Overdraft = overdraft
    member x.Balance = balance

    // Future balance predicted by sequential calculation
    member x.SeqPrediction 
        with get() = seqPred and set(v) = seqPred <- v
    // Future balance predicted by parallel calculation
    member x.ParPrediction 
        with get() = parPred and set(v) = parPred <- v
    // Future balance predicted by PLINQ calculation
    member x.PlinqPrediction 
        with get() = plinqPred and set(v) = plinqPred <- v
    // Future balance predicted by PSeq module calculation
    member x.PSeqPrediction 
        with get() = pseqPred and set(v) = pseqPred <- v

    // Warning flag based on prediction by sequential calculation
    member x.SeqWarning 
        with get() = seqWarn and set(v) = seqWarn <- v
    // Warning flag based on prediction by parallel calculation
    member x.ParWarning 
        with get() = parWarn and set(v) = parWarn <- v
    // Warning flag based on prediction by PLINQ calculation
    member x.PlinqWarning 
        with get() = plinqWarn and set(v) = plinqWarn <- v
    // Warning flag based on prediction by PSeq module calculation
    member x.PSeqWarning 
        with get() = pseqWarn and set(v) = pseqWarn <- v


    /// Assign balance history to vary randomly around randomly assigned trend
    member x.AssignRandomTrend(goodBalance:Trend, badBalance:Trend, variation, random:Random) =
        // Choose random trend
        let rateMean = (goodBalance.Slope + badBalance.Slope) / 2.0
        let initialBalanceMean = (goodBalance.Intercept + badBalance.Intercept) / 2.0
        let rate = rateMean + rateScale * random.NextDouble()
        let initialBalance = initialBalanceMean + balanceScale * random.NextDouble()
        let trend = new Trend(slope = rate, intercept = initialBalance)

        // Balance history is trend plus noise
        for i in 0 .. balance.Length - 1 do
            balance.[i] <- trend.Predict(float i) + variation * random.NextDouble()


    /// Print balances for months starting at firstMonth
    member x.PrintBalance(firstMonth, months) =
      for month in firstMonth .. firstMonth + months - 1 do
          if month < balance.Length then
              Console.Write("{0,9:F}", balance.[month]);
          else
              Console.Write("        "); // line up columns even if data missing
