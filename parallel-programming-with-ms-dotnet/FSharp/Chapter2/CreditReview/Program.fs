// ===============================================================================
//  Microsoft patterns & practices
//  Parallel Programming Guide
// ===============================================================================
//  Copyright © Microsoft Corporation.  All rights reserved.
//  This code released under the terms of the 
//  Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
// ===============================================================================
module Microsoft.Practices.ParallelGuideSamples.CreditReview.Program

open System
open System.Globalization
open System.Linq
open System.Threading.Tasks
open Microsoft.FSharp.Collections
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.CreditReview.Prediction

// ------------------------------------------------------------------------------
// Usage: CreditReview n, optional n is number of customers, use 100,000+ for meaningful timings
// ------------------------------------------------------------------------------

[<EntryPoint>]
let main (args:_[]) = 
    Console.WriteLine("Credit Review Sample\n")
#if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n")
#endif

    // Seed 1 makes runs reproducible
    let random = new Random(1)

    // Defaults for data generation, may override some on command line
    let goodBalance = new Trend(slope = 0.0, intercept = 0.0)
    let badBalance = new Trend(slope = -150.0, intercept = 0.0)
    
    let variation = 100.0
    let overdraft = -1000.0 // Default overdraft limit

    // Printed table of results
    let rows = 8
    let cols = 4

    // Optionally override some defaults on command line
    // For data runs make big enough for significant timing measurements
    let customerCount =
      if args.Length <= 0 then 1000000
      else Int32.Parse(args.[0], CultureInfo.CurrentCulture)

    let months =
      if args.Length <= 1 then 36
      else 
        let m = Int32.Parse(args.[1], CultureInfo.CurrentCulture)
        // PrintBalance requires at least 4 months
        if m < 4 then 4 else m

    // Force JIT compilation before timing tests
    let fewCustomers = 10
    let fewMonths = 3
    let smallAccounts = new AccountRepository(fewCustomers, fewMonths, overdraft)
    smallAccounts.AssignRandomTrends(goodBalance, badBalance, variation, random)

    updatePredictionsSequential(smallAccounts)
    updatePredictionsParallel(smallAccounts)
    updatePredictionsPlinq(smallAccounts)
    updatePredictionsPSeq(smallAccounts)

    // Create accounts for timing tests
    let accounts = new AccountRepository(customerCount, months, overdraft)
    accounts.AssignRandomTrends(goodBalance, badBalance, variation, random)

    // Print summary of accounts  
    Console.WriteLine()
    Console.WriteLine("{0} customers, {1} months in each account", customerCount, months)

    // Execute sequential and parallel versions, print timings
    Console.WriteLine()
    SampleUtilities.TimedRun "Sequential" (fun () ->
        updatePredictionsSequential(accounts)
        customerCount) |> ignore
    SampleUtilities.TimedRun "  Parallel" (fun () ->
        updatePredictionsParallel(accounts)
        customerCount ) |> ignore
    SampleUtilities.TimedRun "     PLINQ" (fun () ->
        updatePredictionsPlinq(accounts) 
        customerCount ) |> ignore
    SampleUtilities.TimedRun "   F# PSeq" (fun () ->
        updatePredictionsPSeq(accounts) 
        customerCount ) |> ignore

    // Print a few accounts including predictions and warnings
    accounts.Print(rows, months - cols, cols) // print the last few months
    Console.WriteLine("\nRun complete... press enter to finish.")
    Console.ReadLine() |> ignore
    0
