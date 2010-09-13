//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Collections.Generic;
using Microsoft.Practices.ParallelGuideSamples.Utilities;
using System.Globalization;
using System.Collections.ObjectModel;

using SubscriberID = System.Int32;

namespace Microsoft.Practices.ParallelGuideSamples.SocialNetwork
{
    using IDMultisetItemList = Collection<KeyValuePair<SubscriberID, int>>;  // to sort, must use list of Multiset items not Multiset itself

    class Program
    {
        /// <summary>
        /// Usage: SocialNework subscriberCount maxFriends
        /// Both arguments are optional, defaults are 1000 and 10.
        /// </summary>
        static void Main(string[] args)
        {
            Console.WriteLine("Social Network Sample\n");
#if DEBUG
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif

            Random random = new Random(1);  // seed for random but reproducible runs

            // Defaults for data generation, may override some on command line
            int subscriberCount = 10000;
            int maxFriends = 2000;

            // Defaults for printed table of results
            const int maxRows = 16;
            const int maxCols = 8;
            const int maxCandidates = maxRows; 

            // Optionally override some defaults on command line
            if (args.Length > 0) subscriberCount = Int32.Parse(args[0], CultureInfo.CurrentCulture);
            if (args.Length > 1) maxFriends = Int32.Parse(args[1], CultureInfo.CurrentCulture);

            Console.WriteLine("Creating data...");
            // Allocate subscribers, assign friends for timing tests
            SubscriberRepository subscribers = new SubscriberRepository(subscriberCount);
            subscribers.AssignRandomFriends(maxFriends, random);

            // Print a few subscribers and a summary
            Console.WriteLine();
            Console.WriteLine("Some subscribers and some of their friends");
            subscribers.Print(maxRows, maxCols);
            Console.WriteLine();
            Console.WriteLine("{0} subscribers in all, with up to {1} friends or even more  ", subscriberCount, maxFriends);

            // Choose a subscriber seeking friends
            const SubscriberID id = 0;
            var subscriber = subscribers.GetSubscriber(id);
            Console.WriteLine();
            Console.WriteLine("Find potential friends for this subscriber, with these friends:");
            Console.Write("{0,10:D}", id);
            subscriber.Print(subscriber.Friends.Count);
            Console.WriteLine();

            // Sequential for
            var candidates = new IDMultisetItemList(); // to sort, must use list of Multiset items not Multiset itself
            SampleUtilities.TimedRun(() =>
                {
                    candidates = subscribers.PotentialFriendsSequential(id, maxCandidates);
                    return candidates.Count;
                },
                "  Sequential for");
            Console.WriteLine();

            int rows = Math.Min(maxRows, candidates.Count);
            Console.WriteLine("{0} potential friends for this subscriber, and the number of mutual friends", rows);            
            Multiset.Print(candidates);
            Console.WriteLine();

            // Parallel.ForEach 
            SampleUtilities.TimedRun(() =>
                {
                    candidates = subscribers.PotentialFriendsParallel(id, maxCandidates);
                    return candidates.Count;
                },
                "Parallel.ForEach");
            Console.WriteLine();

            rows = Math.Min(maxRows, candidates.Count);
            Console.WriteLine("{0} potential friends for this subscriber, and the number of mutual friends", rows);
            Multiset.Print(candidates);
            Console.WriteLine();

            // Sequential LINQ
            SampleUtilities.TimedRun(() =>
                {
                    candidates = subscribers.PotentialFriendsLinq(id, maxCandidates);
                    return candidates.Count;
                },
                " Sequential LINQ");
            Console.WriteLine();

            rows = Math.Min(maxRows, candidates.Count);
            Console.WriteLine("{0} potential friends for this subscriber, and the number of mutual friends", rows);
            Multiset.Print(candidates);
            Console.WriteLine();

            // PLINQ
            SampleUtilities.TimedRun(() =>
                {
                    candidates = subscribers.PotentialFriendsPLinq(id, maxCandidates);
                    return candidates.Count;
                },
                "           PLINQ");
            Console.WriteLine();

            rows = Math.Min(maxRows, candidates.Count);
            Console.WriteLine("{0} potential friends for this subscriber, and the number of mutual friends", rows);
            Multiset.Print(candidates);

            Console.WriteLine("\nRun complete... press enter to finish."); 
            Console.ReadLine();
        }
    }
}
