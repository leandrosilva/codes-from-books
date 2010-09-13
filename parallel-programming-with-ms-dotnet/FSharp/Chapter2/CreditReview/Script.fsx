// ===============================================================================
//  Microsoft patterns & practices
//  Parallel Programming Guide
// ===============================================================================
//  Copyright © Microsoft Corporation.  All rights reserved.
//  This code released under the terms of the 
//  Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
// ===============================================================================

#r @"..\..\Utilities\bin\Release\Utilities.dll"
#load @"..\..\PSeq.fs"

#load "Account.fs"
#load "AccountRepository.fs"
#load "Prediction.fs"

open System
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.CreditReview
open Microsoft.Practices.ParallelGuideSamples.CreditReview.Prediction


// Seed 1 makes runs reproducible
let random = new Random(1)

// Defaults for data generation, may override some on command line
let goodBalance = new Trend(slope = 0.0, intercept = 0.0)
let badBalance = new Trend(slope = -150.0, intercept = 0.0)
    
let variation = 100.0
let overdraft = -1000.0 // Default overdraft limit


// *** TODO ***
// You can change the parameters of the test here
// Note: PrintBalance requires at least 4 months

let customerCount = 1000000
let months = 36

// Create accounts for timing tests
let accounts = new AccountRepository(customerCount, months, overdraft)
accounts.AssignRandomTrends(goodBalance, badBalance, variation, random)

// Turn on timing in F# Interactive and run various versions of the function
// NOTE: Run the sample multiple times to force JIT compilation of functions
#time "on"

// Sequential using for loop
updatePredictionsSequential(accounts)
// Parallel using Parallel.For
updatePredictionsParallel(accounts)
// Parallel using PLINQ
updatePredictionsPlinq(accounts) 
// Parallel using PSeq module (F# specific)
updatePredictionsPSeq(accounts) 

#time "off"

// Print a few accounts including predictions and warnings (last few months)
let rows = 8
let cols = 4
accounts.Print(rows, months - cols, cols)

