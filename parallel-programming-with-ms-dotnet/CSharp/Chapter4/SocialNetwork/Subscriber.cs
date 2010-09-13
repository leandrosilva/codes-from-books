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

using SubscriberID = System.Int32;

namespace Microsoft.Practices.ParallelGuideSamples.SocialNetwork
{
    /// <summary>
    /// A subscriber's data in the social network
    /// For our present purposes, just a collection of friends' IDs 
    /// </summary>
    public class Subscriber
    {
        readonly HashSet<SubscriberID> friends;

        public Subscriber()
        {
            friends = new HashSet<SubscriberID>();
        }

        /// <summary>
        /// Return a reference for read-only operations
        /// </summary>
        public HashSet<SubscriberID> Friends { get { return friends; } }

        /// <summary>
        /// Return a copy
        /// </summary>
        public HashSet<SubscriberID> FriendsCopy()
        {
            return new HashSet<SubscriberID>(friends);
        }

        public void Print(int maxFriends)
        {
            Console.Write("{0,5:D}", Friends.Count);
            var subs = Friends.GetEnumerator();
            for (int i = 0; i < Math.Min(maxFriends, Friends.Count); i++) 
            {
                subs.MoveNext();
                Console.Write("{0,8:D}", subs.Current);
            }
            Console.WriteLine();
        }
    }
}
