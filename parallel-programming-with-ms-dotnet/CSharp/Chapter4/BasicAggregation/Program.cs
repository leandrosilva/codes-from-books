//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Linq;
using Microsoft.Practices.ParallelGuideSamples.Utilities;
using System.Threading.Tasks;
using System.Collections.Concurrent;

namespace Microsoft.Practices.ParallelGuideSamples.BasicAggregation
{
    class Program
    {
        const int SequenceSize = 100000000;
        static void Main()
        {
            Task.Factory.StartNew(() => MainTask()).Wait();
        }

        static void MainTask()
        {
            Console.WriteLine("Basic Aggregation Samples\n");
#if DEBUG
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif
            var sequence = SampleUtilities.Range(SequenceSize);

            SampleUtilities.TimedAction(() => Chapter4Sample01Sequential(sequence), "calculate sum, sequential for loop");
            GC.Collect();
            SampleUtilities.TimedAction(() => Chapter4Sample01IncorrectParallel(sequence), "calculate sum, incorrectly coded parallel loop");
            GC.Collect();
            SampleUtilities.TimedAction(() => Chapter4Sample02Linq(sequence), "calculate sum, LINQ (sequential)");
            GC.Collect();
            SampleUtilities.TimedAction(() => Chapter4Sample02Plinq(sequence), "calculate sum, PLINQ (parallel)");
            GC.Collect();
            SampleUtilities.TimedAction(() => Chapter4Sample03Plinq(sequence), "custom aggregation (product) PLINQ (parallel)");
            GC.Collect();             
            SampleUtilities.TimedAction(() => Chapter4Sample01Parallel(sequence), "calculate sum, parallel for each");
            GC.Collect();
            SampleUtilities.TimedAction(() => Chapter4Sample01ParallelPartitions(sequence), "calculate sum, parallel partitions");
            GC.Collect();

            Console.WriteLine("\nRun complete... press enter to finish.");
            Console.ReadLine();
        }

        // general transformation before calculating aggregate sum
        static double Normalize(double x)
        {
            return x;
        }

        static double Chapter4Sample01Sequential(double[] sequence)
        {
            double sum = 0.0d;
            for (int i = 0; i < sequence.Length; i++)
            {
                sum += Normalize(sequence[i]);
            }
            return sum;
        }

        // WARNING: BUGGY CODE. Do not copy this method.
        // This version will run *much slower* than the sequential version
        static double Chapter4Sample01IncorrectParallel(double[] sequence)
        {
            object lockObject = new object();
            double sum = 0.0d;

            // BUG -- Do not use Parallel.For 
            Parallel.For(0, sequence.Length, i =>
            {
                // BUG -- Do not use locking inside of a parallel loop for aggregation
                lock (lockObject)
                {
                    // BUG -- Do not use shared variable for parallel aggregation
                    sum += Normalize(sequence[i]);
                }
            });
            return sum;
        }

        static double Chapter4Sample02Linq(double[] sequence)
        {
            return (from x in sequence select Normalize(x)).Sum();
        }

        static double Chapter4Sample02Plinq(double[] sequence)
        {
            return (from x in sequence.AsParallel() select Normalize(x)).Sum();
        }

        static double Chapter4Sample03Plinq(double[] sequence)
        {
            return (from x in sequence.AsParallel() select Normalize(x)) 
                        .Aggregate(1.0d, (y1, y2) => y1 * y2);
        }

        static double Chapter4Sample01Parallel(double[] sequence)
        {
            object lockObject = new object();
            double sum = 0.0d;

            // ForEach<TSource, TLocal>(
            //   IEnumerable<TSource> source, 
            //   Func<TLocal> localInit, 
            //   Func<TSource, ParallelLoopState, TLocal, TLocal> body, 
            //   Action<TLocal> localFinally);
            Parallel.ForEach(
                // 1- The values to be aggregated
              sequence,

              // 2- The local initial partial result
              () => 0.0d,

              // 3- The loop body
              (x, loopState, partialResult) =>
              {
                  return Normalize(x) + partialResult;
              },

              // 4- The final step of each local context            
              (localPartialSum) =>
              {
                  // Enforce serial access to single, shared result
                  lock (lockObject)
                  {
                      sum += localPartialSum;
                  }
              });
            return sum;
        }

        static double Chapter4Sample01ParallelPartitions(double[] sequence)
        {
            object lockObject = new object();
            double sum = 0.0d;
            var rangePartitioner = Partitioner.Create(0, sequence.Length);

            // ForEach<TSource, TLocal>(
            //   Partitioner<TSource> source, 
            //   Func<TLocal> localInit, 
            //   Func<TSource, ParallelLoopState, TLocal, TLocal> body, 
            //   Action<TLocal> localFinally);
            Parallel.ForEach(
                // 1- the input intervals
              rangePartitioner,

              // 2- The local initial partial result
              () => 0.0,

              // 3- The loop body for each interval
              (range, loopState, initialValue) =>
              {
                  double partialSum = initialValue;
                  for (int i = range.Item1; i < range.Item2; i++)
                  {
                      partialSum += Normalize(sequence[i]);
                  }
                  return partialSum;
              },

              // 4- The final step of each local context
              (localPartialSum) =>
              {
                  // Use lock to enforce serial access to shared result
                  lock (lockObject)
                  {
                      sum += localPartialSum;
                  }
              });
            return sum;
        }
    }
}
