// ===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
// ===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
// ===============================================================================

using System;
using System.Collections.Generic;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.CreditReview
{
    using AccountRecord = KeyValuePair<int, Account>;

	/// <summary>
    /// Repository of customer accounts
    /// </summary>
    class AccountRepository
    {
        /// <summary>
        /// Repository is implemented by a dictionary from customer account numbers to account data,
        /// an array of monthly balances etc.
        /// </summary>
        readonly Dictionary<int, Account> accounts = new Dictionary<int, Account>();

        /// <summary>
        /// Constructor, allocate account for customerCount customers, each with months balance history
        /// </summary>
        public AccountRepository(int customerCount, int months, double overdraft)
        {
            for (int customer = 0; customer < customerCount; customer++)
            {
                accounts[customer] = new Account(months, overdraft);
            }
        }

        /// <summary>
        /// Assign every account with monthly balances that fit randomly assigned trend
        /// </summary>
        public void AssignRandomTrends(Trend goodBalance, Trend badBalance, double variation, Random random)
        {
            foreach (AccountRecord record in accounts)
            {
                var account = record.Value;
                account.AssignRandomTrend(goodBalance, badBalance, variation, random);
            }
        }

        /// <summary>
        /// Property that returns collection of all accounts in repository
        /// </summary>
        public IEnumerable<Account> AllAccounts
        {
            get { return accounts.Values; }
        }

        /// <summary>
        /// Print first rows accounts from firstMonth for months, including predictions and warnings
        /// </summary>
        public void Print(int rows, int firstMonth, int months)
        {
            // Print column headings
            Console.WriteLine();
            Console.WriteLine("Customer   Recent balances for month number   Predicted balances and warnings");
            Console.Write("        ");
            for (int month = firstMonth; month < firstMonth + months; month++) 
            { 
                Console.Write("{0,9:D}", month); 
            }
            Console.WriteLine("      Seq.    Parallel       PLINQ\n");

            // Print results for first nRows customers
            for (int customer = 0; customer < rows; customer++)
            {
                if (accounts.ContainsKey(customer))
                {
                    Console.Write("{0,7:0.#} ", customer);
                    var acc = accounts[customer];
                    acc.PrintBalance(firstMonth, months);
                    Console.WriteLine("  {0,8:F} {1}  {2,8:F} {3}  {4,8:F} {5}",
                        acc.SeqPrediction, acc.SeqWarning ? 'W' : ' ',
                        acc.ParPrediction, acc.ParWarning ? 'W' : ' ',
                        acc.PlinqPrediction, acc.PlinqWarning ? 'W' : ' ');
                }
            }
        }
    }
}
