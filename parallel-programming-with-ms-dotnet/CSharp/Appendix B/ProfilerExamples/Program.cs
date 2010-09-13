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
using System.Threading;
using System.Threading.Tasks;

namespace Microsoft.Practices.ParallelGuideSamples.ProfilerExamples
{
	class Program
	{
		static void Main(string[] args)
		{
			Console.WriteLine("Profiler Samples\n");

            Console.WriteLine("Press any key to start run after the profiler has initiliazed...");
            Console.ReadKey();

			Console.WriteLine("Starting...\n");

			if (args.Length != 1)
			{
				Help();
				return;
			}

			switch (args[0].Trim().ToLower())
			{
				case "deadlock":
					Console.WriteLine("Showing deadlock.");
					Console.WriteLine("WARNING: This program does not terminate!");
					Deadlock();
					break;
				case "lockcontention":
					Console.WriteLine("Showing lock contention.");
					LockContention();
					break;
				case "oversubscription":
					Console.WriteLine("Showing oversubscription.");
					Oversubscription();
					break;
				case "loadimbalance":
					Console.WriteLine("Showing load imbalance.");
					LoadImbalance();
					break;
				default:
					Help();
					break;
			}

			Console.WriteLine("\nRun complete...");
		}

		static void Help()
		{
			Console.WriteLine("Usage: [deadlock|lockcontention|oversubscription|loadimbalance]");
		}

		static void Deadlock()
		{
			object obj1 = new object();
			object obj2 = new object();

			Parallel.Invoke(
				() =>
				{
					for (int i = 0; ; i++)
					{
						lock (obj1)
						{
							Console.WriteLine("Got 1 at {0}", i);
							lock (obj2) Console.WriteLine("Got 2 at {0}", i);
						}
					}
				},
				() =>
				{
					for (int i = 0; ; i++)
					{
						lock (obj2)
						{
							Console.WriteLine("Got 2 at {0}", i);
							lock (obj1) Console.WriteLine("Got 1 at {0}", i);
						}
					}
				});
		}

		static void LockContention()
		{
			object syncObj = new object();

			for (int p = 0; p < Environment.ProcessorCount; p++)
			{
				new Thread(() =>
				{
					for (int i = 0; i < 50; i++)
					{
						// Do work
						for (int j = 0; j < 1000; j++) ;

						// Do protected work
						lock (syncObj)
							for (int j = 0; j < 100000000; j++) ;
					}
				}).Start();
			}
		}

		static void Oversubscription()
		{
			for (int i = 0; i < (Environment.ProcessorCount * 4); i++)
			{
				new Thread(() =>
				{
					// Do work 
					for (int j = 0; j < 1000000000; j++) ;
				}).Start();
			}
		}

		static void LoadImbalance()
		{
			const int loadFactor = 10;
			ParallelEnumerable.Range(0, 100000).ForAll(i =>
			{
				// Do work
				for (int j = 0; j < (i * loadFactor); j++) ;
			});
		}
	}
}
