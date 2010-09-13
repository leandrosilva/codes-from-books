// ===============================================================================
//  Microsoft patterns & practices
//  Parallel Programming Guide
// ===============================================================================
//  Copyright © Microsoft Corporation.  All rights reserved.
//  This code released under the terms of the 
//  Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
// ===============================================================================

using System;
using System.Globalization;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.CreditReview
{
    class Program
    {
        private const int NumberOfMonths = 3;

        static void UpdatePredictionsSequential(AccountRepository accounts)
        {
            foreach (Account account in accounts.AllAccounts)
            {
                Trend trend = SampleUtilities.Fit(account.Balance);
                double prediction = trend.Predict(account.Balance.Length + NumberOfMonths); 
                account.SeqPrediction = prediction;
                account.SeqWarning = prediction < account.Overdraft;
            }
        }

        static void UpdatePredictionsParallel(AccountRepository accounts)
        {
            Parallel.ForEach(accounts.AllAccounts, account =>
            {
                Trend trend = SampleUtilities.Fit(account.Balance);
                double prediction = trend.Predict(account.Balance.Length + NumberOfMonths);
                account.ParPrediction = prediction;
                account.ParWarning = prediction < account.Overdraft;
            });
        }

        static void UpdatePredictionsPlinq(AccountRepository accounts)
        {            
            accounts.AllAccounts
                .AsParallel()
                .ForAll(account =>
                    {
                        Trend trend = SampleUtilities.Fit(account.Balance);
                        double prediction = trend.Predict(account.Balance.Length + NumberOfMonths);
                        account.PlinqPrediction = prediction;
                        account.PlinqWarning = prediction < account.Overdraft;         
                    });
        }

        /// <summary>
        /// Usage: CreditReview n, optional n is number of customers, use 100,000+ for meaningful timings
        /// </summary>
        static void Main(string[] args)
        {
            Console.WriteLine("Credit Review Sample\n");
#if DEBUG
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif

            Random random = new Random(1);  // seed 1 makes runs reproducible

            // Defaults for data generation, may override some on command line
            int months = 36;
            int customerCount = 1000000; // for data runs make big enough for significant timing measurements
            Trend goodBalance = new Trend { Slope = 0.0, Intercept = 0.0 };
            Trend badBalance = new Trend { Slope = -150.0, Intercept = 0.0 };
            const double variation = 100.0;
            const double overdraft = -1000.0; // Default overdraft limit

            // Printed table of results
            const int rows = 8;
            const int cols = 4;

            // Optionally override some defaults on command line
            if (args.Length > 0) 
                customerCount = Int32.Parse(args[0], CultureInfo.CurrentCulture);
            if (args.Length > 1) 
                months = Int32.Parse(args[1], CultureInfo.CurrentCulture);
            if (months < 4) 
                months = 4; // PrintBalance requires at least 4 months

            // Force JIT compilation before timing tests
            const int fewCustomers = 10;
            const int fewMonths = 3;
            AccountRepository smallAccounts = new AccountRepository(fewCustomers, fewMonths, overdraft);
            smallAccounts.AssignRandomTrends(goodBalance, badBalance, variation, random);
            UpdatePredictionsSequential(smallAccounts);
            UpdatePredictionsParallel(smallAccounts);
            UpdatePredictionsPlinq(smallAccounts);

            // Create accounts for timing tests
            AccountRepository accounts = new AccountRepository(customerCount, months, overdraft);
            accounts.AssignRandomTrends(goodBalance, badBalance, variation, random);

            // Print summary of accounts  
            Console.WriteLine();
            Console.WriteLine("{0} customers, {1} months in each account", customerCount, months);

            // Execute sequential and parallel versions, print timings
            Console.WriteLine();
            SampleUtilities.TimedRun(
                () => { UpdatePredictionsSequential(accounts); return customerCount; }, "Sequential");
            SampleUtilities.TimedRun(
                () => { UpdatePredictionsParallel(accounts); return customerCount; }, "  Parallel");
            SampleUtilities.TimedRun(
                () => { UpdatePredictionsPlinq(accounts); return customerCount; }, "     PLINQ");

            // Print a few accounts including predictions and warnings
            accounts.Print(rows, months - cols, cols); // print the last few months

            Console.WriteLine("\nRun complete... press enter to finish.");
            Console.ReadLine();
        }
    }
}
