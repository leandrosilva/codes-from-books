// ===============================================================================
//  Microsoft patterns & practices
//  Parallel Programming Guide
// ===============================================================================
//  Copyright © Microsoft Corporation.  All rights reserved.
//  This code released under the terms of the 
//  Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
// ===============================================================================
module Microsoft.Practices.ParallelGuideSamples.CreditReview.Prediction

open System
open System.Globalization
open System.Linq
open System.Threading.Tasks
open Microsoft.FSharp.Collections
open Microsoft.Practices.ParallelGuideSamples.CreditReview
open Microsoft.Practices.ParallelGuideSamples.Utilities

// ------------------------------------------------------------------------------
// Calculating predictions
// ------------------------------------------------------------------------------

let numberOfMonths = 3

/// Calculate prediction for all accounts using sequential for loop
let updatePredictionsSequential(accounts:AccountRepository) =
    for account in accounts.AllAccounts do
        let trend = SampleUtilities.FitImplicit(account.Balance)
        let prediction = trend.Predict(float (account.Balance.Length + numberOfMonths))
        account.SeqPrediction <- prediction
        account.SeqWarning <- prediction < account.Overdraft


/// Calculate prediction for all accounts using Parallel.ForEach loop
let updatePredictionsParallel(accounts:AccountRepository) =
    Parallel.ForEach(accounts.AllAccounts, fun (account:Account) ->
        let trend = SampleUtilities.FitImplicit(account.Balance)
        let prediction = trend.Predict(float (account.Balance.Length + numberOfMonths))
        account.ParPrediction <- prediction
        account.ParWarning <- prediction < account.Overdraft ) |> ignore


/// Calculate prediction for all accounts using PLINQ query operators
let updatePredictionsPlinq(accounts:AccountRepository) =
    accounts
      .AllAccounts
      .AsParallel()
      .ForAll(fun account ->
          let trend = SampleUtilities.FitImplicit(account.Balance)
          let prediction = trend.Predict(float (account.Balance.Length + numberOfMonths))
          account.PlinqPrediction <- prediction
          account.PlinqWarning <- prediction < account.Overdraft )

/// Calculate prediction for all accounts using PSeq module (F# specific)
let updatePredictionsPSeq(accounts:AccountRepository) =
    accounts.AllAccounts
      |> PSeq.iter (fun account ->
          let trend = SampleUtilities.FitImplicit(account.Balance)
          let prediction = trend.Predict(float (account.Balance.Length + numberOfMonths))
          account.PSeqPrediction <- prediction
          account.PSeqWarning <- prediction < account.Overdraft )
