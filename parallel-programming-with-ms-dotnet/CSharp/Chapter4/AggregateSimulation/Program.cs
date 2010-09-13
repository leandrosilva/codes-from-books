//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.AggregateSimulation
{
    class Program
    {       
        const int TableSize = 40;
        const double BucketSize = 5;

        static void Main()
        {
            Console.WriteLine("Aggregation Sample\n");
#if DEBUG
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif 
            const int trialCount = 5000000;
            const double mean = 102.5;
            const double stdDev = 15;

            int[] histogram1, histogram2, histogram3;

            Stopwatch s = new Stopwatch();

            Console.WriteLine("Performing Sequential Aggregation...");
            s.Start();
            histogram1 = DoSequentialAggregation(trialCount, mean, stdDev);
            s.Stop();
            Console.WriteLine(SampleUtilities.FormattedTime(s.Elapsed));
            PrintHistogram(histogram1);

            Console.WriteLine("Performing Parallel Aggregation...");
            s.Restart();
            histogram2 = DoParallelAggregation(trialCount, mean, stdDev);
            s.Stop();
            Console.WriteLine(SampleUtilities.FormattedTime(s.Elapsed));
            PrintHistogram(histogram2);

            Console.WriteLine("Performing PLINQ Aggregation...");
            s.Restart();
            histogram3 = DoParallelAggregationPlinq(trialCount, mean, stdDev);
            s.Stop();
            Console.WriteLine(SampleUtilities.FormattedTime(s.Elapsed));
            PrintHistogram(histogram3);

            Console.WriteLine("\nRun complete... press enter to finish.");
            Console.ReadLine();
        }

        static int[] DoSequentialAggregation(int count, double mean, double stdDev)
        {
            var generator = new Random(SampleUtilities.MakeRandomSeed());

            int[] histogram = MakeEmptyHistogram();

            for (int i = 0; i < count; i++)
            {
                // get the next input value
                var sample = generator.NextDouble();
                if (sample > 0.0)
                {            
                    // MAP: perform a simulation trial for the sample value
                    var simulationResult = DoSimulation(sample, mean, stdDev);

                    // REDUCE: merge the result of simulation into a histogram
                    int histogramBucket = (int)Math.Floor(simulationResult / BucketSize);
                    if (0 <= histogramBucket && histogramBucket < TableSize)
                        histogram[histogramBucket] += 1;
                }
            }
            return histogram;
        }

        static int[] MakeEmptyHistogram()
        {
            return new int[TableSize];
        }

        static int[] CombineHistograms(int[] histogram1, int[] histogram2)
        {
            var mergedHistogram = MakeEmptyHistogram();
            for (int i = 0; i < TableSize; i++)
                mergedHistogram[i] = histogram1[i] + histogram2[i];
            return mergedHistogram;
        }
            
        // Placeholder for a user-written simulation routine. For example, this 
        // could be a financial simulation that explores various risk outcomes.
        // This placeholder just transforms the value so that the outputs of
        // simulation will follow a bell curve.
        static double DoSimulation(double sampleValue, double mean, double stdDev)
        {
            return SampleUtilities.GaussianInverse(sampleValue, mean, stdDev);
        }

        static int[] DoParallelAggregation(int count, double mean, double stdDev)
        {
            // Partition the iterations
            var rangePartitioner = Partitioner.Create(0, count);

            int[] histogram = MakeEmptyHistogram();

            // ForEach<TSource, TLocal>(Partitioner<TSource> source, 
            //                          Func<TLocal> localInit, 
            //                          Func<TSource, ParallelLoopState, TLocal, TLocal> body, 
            //                          Action<TLocal> localFinally);
            Parallel.ForEach(
                // 1- the partitioner object
                rangePartitioner,

                // 2- the local state object that will 
                // collect results for a single partition
                () => MakeEmptyHistogram(),

                // 3- the body that will be invoked once for each of 
                // the partitions created by the partitioner. (The number of
                // partitions depends on the number of cores.)
                (range, loopState, index, localHistogram) =>
                {
                    // Make a local generator to avoid conflicts between partitions.
                    var generator = new Random(SampleUtilities.MakeRandomSeed());

                    // Iterate over the range assigned by the partitioner
                    for (int i = range.Item1; i < range.Item2; i++)
                    {
                        // With each iteration get the next input value 
                        var sample = generator.NextDouble();

                        if (sample > 0.0)
                        {
                            // MAP: perform a simulation trial for the sample value
                            var simulationResult = DoSimulation(sample, mean, stdDev);

                            // REDUCE: merge the result of simulation into the local histogram
                            int histogramBucket = (int)Math.Floor(simulationResult / BucketSize);
                            if (0 <= histogramBucket && histogramBucket < TableSize)
                                localHistogram[histogramBucket] += 1;
                        }
                    }
                    return localHistogram;
                },

                // 4- The finalizer that will be run once for each partition to combine
                // results created for each partition.
                (localHistogram) =>
                {
                    // Use lock to enforce serial access to single, shared result
                    lock (histogram)
                    {
                        // MERGE: local histogram results are added into the global histogram
                        for (int i = 0; i < TableSize; i++)
                            histogram[i] += localHistogram[i];
                    }
                }
            ); // Parallel.ForEach

            // return the global histogram
            return histogram;
        }

        static int[] DoParallelAggregationPlinq(int count, double mean, double stdDev)
        {
            // Aggregate<TSource, TAccumulate, TResult>(
            //   this ParallelQuery<TSource> source, 
            //   Func<TAccumulate> seedFactory, 
            //   Func<TAccumulate, TSource, TAccumulate> updateAccumulatorFunc, 
            //   Func<TAccumulate, TAccumulate, TAccumulate> combineAccumulatorsFunc, 
            //   Func<TAccumulate, TResult> resultSelector);        
            return ParallelEnumerable.Range(0, count).Aggregate(
                // 1- create an empty local accumulator object
                //    that includes all task-local state
                () => new Tuple<int[], Random>(MakeEmptyHistogram(), 
                                               new Random(SampleUtilities.MakeRandomSeed())),

                // 2- run the simulation, adding result to local accumulator 
                (localAccumulator, i) =>
                {
                   // With each iteration get the next random value 
                   var sample = localAccumulator.Item2.NextDouble();

                   if (sample > 0.0 && sample < 1.0)
                   {
                     // Perform a simulation trial for the sample value
                     var simulationResult = DoSimulation(sample, mean, stdDev);

                     // Put the result of simulation into the histogram of the local accumulator
                     int histogramBucket = (int)Math.Floor(simulationResult / BucketSize);
                     if (0 <= histogramBucket && histogramBucket < TableSize)
                         localAccumulator.Item1[histogramBucket] += 1;                       
                    }
                    return localAccumulator;
                },

                // 3- Combine local results pairwise.
                (localAccumulator1, localAccumulator2) =>
                {
                    return new Tuple<int[], Random>(
                        CombineHistograms(localAccumulator1.Item1, 
                                          localAccumulator2.Item1), 
                        null);
                },

                // 4- Extract answer from final combination
                finalAccumulator => finalAccumulator.Item1
            ); // Aggregate
        }

        static void PrintHistogram(int[] histogram)
        {
            for (int j = 0; j < TableSize; j++)
            {
                Console.WriteLine("{0}; {1}", (int)(j * BucketSize), histogram[j]);
            }
        }
    }
}
