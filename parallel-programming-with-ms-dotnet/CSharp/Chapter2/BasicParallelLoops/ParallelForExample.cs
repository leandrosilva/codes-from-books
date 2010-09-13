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
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops
{
    public class ParallelForExample
    {
        public int LoopBodyComplexity = 50;
        public int NumberOfSteps = 10000000;
        public bool VerifyResult = true;
        public bool SimulateInternalError = false;

        class ParallelForExampleException : Exception { }

        #region Harness

        public void DoParallelFor()
        {
            Console.WriteLine("Parallel For Examples (LoopBodyComplexity={0}, NumberOfSteps={1})",
                LoopBodyComplexity, NumberOfSteps);

            RunParallelForExample(Chapter2Example01, "Sequential for");
            RunParallelForExample(Chapter2Example02, "Simple Parallel.For");
            RunParallelForExample(Chapter2Example03, "PLINQ ForAll");

            RunParallelForExample(Chapter2Example04a, "PLINQ 1");
            RunParallelForExample(Chapter2Example04b, "PLINQ 2");

            RunParallelForExample(Chapter2Example06, "Partitioned");
            RunParallelForExample(Chapter2Example07, "Partitioned with fixed ranges");

            Console.WriteLine();
        }

        public void DoParallelForEach()
        {
            Console.WriteLine("Parallel ForEach Examples (LoopBodyComplexity={0}, NumberOfSteps={1})",
                LoopBodyComplexity, NumberOfSteps);

            RunParallelForEachExample(Chapter2Example21, "Sequential foreach");
            RunParallelForEachExample(Chapter2Example22, "Simple Parallel.ForEach");
            RunParallelForEachExample(Chapter2Example23, "PLINQ ForAll");

            RunParallelForEachExample(Chapter2Example24a, "PLINQ 1");
            RunParallelForEachExample(Chapter2Example24b, "PLINQ 2");

            RunParallelForEachExample(Chapter2Example27, "Partitioned with load balancing");

            Console.WriteLine();
        }

        void RunParallelForExample(Func<double[]> action, string label)
        {
            // clean up from previous run
            GC.Collect();
            try
            {
                double[] result = null;

                SampleUtilities.TimedAction(() => { result = action(); }, "  " + label);

                if (VerifyResult)
                {
                    for (int i = 0; i < NumberOfSteps; i++)
                    {
                        EnsureResult(result[i], i);
                    }
                }
            }
            catch (AggregateException ae)
            {
                ae.Handle((e) =>
                    {
                        Console.WriteLine("  {0}: Failed with {1}", label, e.GetType().ToString());
                        return true;
                    });
            }
            catch (ParallelForExampleException ex)
            {
                Console.WriteLine("  {0}: Failed with unaggregated {1}  ", label, ex.GetType().ToString());
            }
            catch (Exception ex)
            {
                Console.WriteLine("  {0}: Failed with unaggregated {1}  ", label, ex.GetType().ToString());
            }
        }

        void RunParallelForEachExample(Func<int[], double[]> action, string label)
        {
            // clean up from previous run
            GC.Collect();
            try
            {
                double[] result = null;
                int[] source = Enumerable.Range(0, NumberOfSteps).ToArray();

                SampleUtilities.TimedAction(() => { result = action(source); }, "  " + label);

                if (VerifyResult)
                {
                    for (int i = 0; i < NumberOfSteps; i++)
                    {
                        EnsureResult(result[i], i);
                    }
                }
            }
            catch (AggregateException ae)
            {
                ae.Handle((e) =>
                {
                    Console.WriteLine("  {0}: Failed with {1}", label, e.GetType().ToString());
                    return true;
                });
            }
            catch (ParallelForExampleException ex)
            {
                Console.WriteLine("  {0}: Failed with unaggregated {1}  ", label, ex.GetType().ToString());
            }
            catch (Exception ex)
            {
                Console.WriteLine("  {0}: Failed with unaggregated {1}  ", label, ex.GetType().ToString());
            }
        }
 
        #endregion

        #region Work Function

        public void EnsureResult(double actual, int i)
        {
            double expected = DoWorkExpectedResult(i);
            if (actual != expected)
                throw new InvalidOperationException(
                    String.Format("Unexpected value: actual {0}, expected {1}",
                                     actual, expected));
        }

        public double DoWorkExpectedResult(int i)
        {
            return 2.5d * (LoopBodyComplexity + 1) * LoopBodyComplexity * i;
        }

        public double DoWork(int i)
        {
            double result = 0;
            for (int j = 1; j < LoopBodyComplexity + 1; j++)
            {
                double j2 = (double)j;
                double i2 = (double)i;
                result += Math.Sqrt((9.0d * i2 * i2 + 16.0d * i * i) * j2 * j2);
            }

            // simulate unexpected condition in loop body
            if (i % 402030 == 2029 && SimulateInternalError)
                throw new ParallelForExampleException();

            return Math.Round(result);
        } 

        #endregion
        
        #region For Examples

        // Sequential for loop
        public double[] Chapter2Example01()
        {
            double[] result = new double[NumberOfSteps];

            for (int i = 0; i < NumberOfSteps; i++)
            {
                result[i] = DoWork(i);
            }
            return result;
        }

        // LINQ 1 (sequential)
        public double[] Chapter2Example01b()
        {
            return Enumerable.Range(0, NumberOfSteps)
                    .Select((i) => DoWork(i)).ToArray();
        }

        // LINQ 2 (sequential)
        public double[] Chapter2Example01c()
        {
            return (from i in Enumerable.Range(0, NumberOfSteps)
                    select DoWork(i)).ToArray();
        }

        public double[] Chapter2Example02()
        {
            double[] result = new double[NumberOfSteps];

            Parallel.For(0, NumberOfSteps, (i) =>
            {
                result[i] = DoWork(i);

            });
            return result;
        }

        public double[] Chapter2Example03()
        {
            double[] result = new double[NumberOfSteps];


            ParallelEnumerable.Range(0, NumberOfSteps)
                 .ForAll((i) => { result[i] = DoWork(i); });

            return result;
        }

        public double[] Chapter2Example04a()
        {
            return (from i in ParallelEnumerable.Range(0, NumberOfSteps).AsOrdered()
                    select DoWork(i)).ToArray();
        }

        public double[] Chapter2Example04b()
        {
            return ParallelEnumerable.Range(0, NumberOfSteps)
                      .AsOrdered()
                      .Select((i) => DoWork(i)).ToArray();

        }

        // optimized for small units of work, each of the same duration
        // avoids false sharing
        // not appropriate if iteration steps of unequal duration
        public double[] Chapter2Example06()
        {
            double[] result = new double[NumberOfSteps];

            Parallel.ForEach(Partitioner.Create(0, NumberOfSteps),
                (range) =>
                {
                    for (int i = range.Item1; i < range.Item2; i++)
                    {
                        result[i] = DoWork(i);
                    }
                });
            return result;
        }

        public double[] Chapter2Example07()
        {
            double[] result = new double[NumberOfSteps];
            int rangeSize = NumberOfSteps / (Environment.ProcessorCount * 10);

            Parallel.ForEach(Partitioner.Create(0, NumberOfSteps, rangeSize >= 1 ? rangeSize : 1),
                (range) =>
                {
                    for (int i = range.Item1; i < range.Item2; i++)
                    {
                        result[i] = DoWork(i);
                    }
                });
            return result;
        } 

        #endregion

        #region ForEachExamples
        
        // Sequential for loop
        public double[] Chapter2Example21(int[] source)
        {
            double[] result = new double[source.Length];

            foreach(var i in source)
            {
                result[i] = DoWork(i);
            }
            return result;
        }

        // LINQ 1 (sequential)
        public double[] Chapter2Example21b(int[] source)
        {
            return source.Select((i) => DoWork(i)).ToArray();
        }

        // LINQ 2 (sequential)
        public double[] Chapter2Example21c(int[] source)
        {
            return (from i in source
                    select DoWork(i)).ToArray();
        }

        public double[] Chapter2Example22(int[] source)
        {
            double[] result = new double[source.Length];

            Parallel.ForEach(source, i =>
            {
                result[i] = DoWork(i);

            });
            return result;
        }

        public double[] Chapter2Example23(int[] source)
        {
            double[] result = new double[source.Length];

            source.AsParallel()
                 .ForAll((i) => { result[i] = DoWork(i); });

            return result;
        }

        public double[] Chapter2Example24a(int[] source)
        {
            return (from i in source.AsParallel().AsOrdered()
                    select DoWork(i)).ToArray();
        }

        public double[] Chapter2Example24b(int[] source)
        {
            return source.AsParallel()
                      .AsOrdered()
                      .Select((i) => DoWork(i)).ToArray();

        }

        // partitioner with load balancing
        public double[] Chapter2Example27(int[] source)
        {
            double[] result = new double[source.Length];
 
            Parallel.ForEach(Partitioner.Create(source, true), i =>
                {
                  result[i] = DoWork(i);
                });
            return result;
        } 

        #endregion

        #region Custom Iterator Example
        
        // using task-local state for Parallel.For iteration
        public double[] Chapter2Example40()
        {
            double[] result = new double[NumberOfSteps];

            Parallel.For(0, NumberOfSteps,
                new ParallelOptions(),
                () => { return new Random(); },
                (i, loopState, random) =>
                {
                    result[i] = random.NextDouble();
                    return random;
                },
                _ => { });

            return result;
        }

        // using task-local state for iteration, with partitioner
        public double[] Chapter2Example41()
        {
            double[] result = new double[NumberOfSteps];

            Parallel.ForEach(Partitioner.Create(0, NumberOfSteps),
                new ParallelOptions(),
                () => { return new Random(); },
                (range, loopState, random) =>
                {
                    for (int i = range.Item1; i < range.Item2; i++)
                        result[i] = random.NextDouble();
                    return random;
                },
                _ => { });

            return result;
        }
 
        #endregion
    }
}
