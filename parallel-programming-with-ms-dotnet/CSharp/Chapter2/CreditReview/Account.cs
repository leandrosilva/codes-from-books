//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.CreditReview
{
    /// <summary>
    /// One customer's account data: array of monthly balances, also predictions and warnings
    /// </summary>
    class Account
    {
        public double[] Balance { get; set; }       // array of monthly balances
        public double Overdraft { get; set; }       // overdraft limit, issue warning if predicted balance is less

        public double SeqPrediction { get; set; }   // predicted future balance from sequential calculation
        public double ParPrediction { get; set; }   // from parallel
        public double PlinqPrediction { get; set; } // from PLINQ 

        public bool SeqWarning { get; set; }        // warning flag based on prediction from sequential calculation
        public bool ParWarning { get; set; }        // from parallel
        public bool PlinqWarning { get; set; }      // from PLINQ 

        /// <summary>
        /// Constructor, allocate balance history for months, assign overdraft
        /// </summary>
        public Account(int months, double overdraft)
        {
            Balance = new double[months];
            Overdraft = overdraft;
        }

        /// <summary>
        /// Assign balance history to vary randomly around randomly assigned trend
        /// </summary>
        public void AssignRandomTrend(Trend goodBalance, Trend badBalance, double variation, Random random)
        {
            // choose random trend
            const double rateScale = 100.0;
            const double balanceScale = 100.0;
            double rateMean = (goodBalance.Slope + badBalance.Slope) / 2;
            double initialBalanceMean = (goodBalance.Intercept + badBalance.Intercept) / 2;
            double rate = rateMean + rateScale * random.NextDouble();
            double initialBalance = initialBalanceMean + balanceScale * random.NextDouble();
            Trend trend = new Trend { Slope = rate, Intercept = initialBalance };

            // balance history is trend plus noise
            for (int i = 0; i < Balance.Length; i++)
            {
            	Balance[i] = trend.Predict(i) + variation * random.NextDouble();
            }
        }

        /// <summary>
        /// Print balances for months starting at firstMonth
        /// </summary>
        public void PrintBalance(int firstMonth, int months)
        {
            for (int month = firstMonth; month < firstMonth + months; month++)
            {
                if (month < Balance.Length)
                {
                    Console.Write("{0,9:F}", Balance[month]);
                }
                else
                {
                    Console.Write("        "); // line up columns even if data missing
                }
            }
            // no WriteLine, may want to print more 
        }
    }
}