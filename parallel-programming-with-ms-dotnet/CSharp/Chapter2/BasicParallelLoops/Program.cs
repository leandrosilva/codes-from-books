//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Threading.Tasks;

namespace Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops
{
    class Program
    {
        static void Main()
        {
            Task.Factory.StartNew(() => MainOnThreadpoolThread()).Wait();
        }

        static void MainOnThreadpoolThread()
        {
            Console.WriteLine("Basic Parallel Loops Samples\n");
#if DEBUG
            Console.WriteLine("For most accurate timing results, use Release build.\n");
            const bool verify = true;
#else   
            const bool verify = false;
#endif
            new CustomIteratorExample().Example();

            var examples = new ParallelForExample[]
                              { 
                                new ParallelForExample() { LoopBodyComplexity = 10000000, NumberOfSteps = 10, VerifyResult = verify },
                                new ParallelForExample() { LoopBodyComplexity = 1000000, NumberOfSteps = 100, VerifyResult = verify },
                                new ParallelForExample() { LoopBodyComplexity = 10000, NumberOfSteps = 10000, VerifyResult = verify },
                                new ParallelForExample() { LoopBodyComplexity = 100, NumberOfSteps = 1000000, VerifyResult = verify },
                                new ParallelForExample() { LoopBodyComplexity = 10, NumberOfSteps = 10000000, VerifyResult = verify }
                              };

            foreach (var e1 in examples)
                e1.DoParallelFor();

            foreach (var e2 in examples)
                e2.DoParallelForEach();

            Console.WriteLine("\nRun complete... press enter to finish."); 
            Console.ReadLine();
        }
    }
}
