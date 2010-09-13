// ===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
// ===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
// ===============================================================================
namespace Microsoft.Practices.ParallelGuideSamples.CreditReview

open System
open System.Collections.Generic
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.CreditReview

type AccountRecord = KeyValuePair<int, Account>

/// Repository of customer accounts
type AccountRepository(customerCount, months, overdraft) = 
    /// Repository is implemented by a dictionary from customer 
    /// account numbers to account data, an array of monthly balances etc.
    let accounts = new Dictionary<int, Account>()

    /// Constructor, allocate account for customerCount 
    /// customers, each with months balance history
    do 
        for customer in 0 .. customerCount - 1 do
            accounts.[customer] <- new Account(months, overdraft)
  
    /// Assign every account with monthly balances 
    /// that fit randomly assigned trend
    member x.AssignRandomTrends(goodBalance, badBalance, variation, random) =
        for record in accounts do
            let account = record.Value
            account.AssignRandomTrend(goodBalance, badBalance, variation, random)

    /// Property that returns collection 
    /// of all accounts in repository
    member x.AllAccounts = accounts.Values

    /// Print first rows accounts from firstMonth for 
    /// months, including predictions and warnings
    member x.Print(rows, firstMonth, months) =
        // Print column headings
        Console.WriteLine()
        Console.WriteLine("Customer   Recent balances for month number   Predicted balances and warnings")
        Console.Write("        ")
        for month in firstMonth .. firstMonth + months - 1 do 
            Console.Write("{0,9:D}", month) 

        Console.WriteLine("      Seq.    Parallel       PLINQ     F# PSeq\n")

        // Print results for first nRows customers
        for customer in 0 .. rows - 1 do
            if accounts.ContainsKey(customer) then
                Console.Write("{0,7:0.#} ", customer)
                let acc = accounts.[customer]
                acc.PrintBalance(firstMonth, months)
                Console.WriteLine
                  ( "  {0,8:F} {1}  {2,8:F} {3}  {4,8:F} {5}  {6,8:F} {7}",
                    acc.SeqPrediction, (if acc.SeqWarning then 'W' else ' '),
                    acc.ParPrediction, (if acc.ParWarning then 'W' else ' '),
                    acc.PlinqPrediction, (if acc.PlinqWarning then 'W' else ' '),
                    acc.PSeqPrediction, (if acc.PSeqWarning then 'W' else ' '))
