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
using System.Linq;
using System.Threading.Tasks;
using System.Collections.ObjectModel;

using SubscriberID = System.Int32;

namespace Microsoft.Practices.ParallelGuideSamples.SocialNetwork
{
    using IDMultiset = Dictionary<SubscriberID, int>;
    using IDMultisetItem = KeyValuePair<SubscriberID, int>;
    using IDMultisetItemList = Collection<KeyValuePair<SubscriberID, int>>;  // to sort, must use list of Multiset items not Multiset itself
    using SubscriberRecord = KeyValuePair<SubscriberID, Subscriber>;

    public class SubscriberRepository
    {
        /// <summary>
        /// Collection of all subscribers in the repository
        /// </summary>
        readonly Dictionary<SubscriberID, Subscriber> subscribers;

        /// <summary>
        /// Constructor, initialize with no subscribers
        /// </summary>
        public SubscriberRepository() { subscribers = new Dictionary<SubscriberID, Subscriber>(); }

        /// <summary>
        /// Constructor, allocate subscribers with no friends
        /// </summary>
        public SubscriberRepository(int subscriberCount)
        {
            subscribers = new Dictionary<SubscriberID, Subscriber>();
            for (SubscriberID subscriber = 0; subscriber < subscriberCount; subscriber++)
            {
                subscribers[subscriber] = new Subscriber();
            }
        }

        /// <summary>
        /// Return number of subscribers
        /// </summary>
        public int Count { get { return subscribers.Count; } }

        /// <summary>
        /// Return subscriber with given ID
        /// </summary>
        /// <param name="id">ID of subscriber</param>
        /// <returns>Subcriber data for that ID</returns>
        public Subscriber GetSubscriber(SubscriberID id)
        {
            return subscribers[id];
        }

        /// <summary>
        /// Assign every subscriber a random number (up to maxFriends) of randomly chosen new friends
        /// Ensure friends relation is symmetric 
        /// </summary> 
        public void AssignRandomFriends(int maxFriends, Random random)
        {
            foreach (SubscriberRecord record in subscribers)
            {
                int id = record.Key;
                Subscriber subscriber = record.Value;
                if (random == null)
                    throw new ArgumentNullException("random");
                int nfriends = random.Next(maxFriends);
                var friends = subscriber.Friends;
                for (int i = 0; i < nfriends; i++)
                {
                    SubscriberID friend = random.Next(subscribers.Count);
                    if (friend != id) // self is never in friends
                    { 
                        friends.Add(friend); // HashSet ensures no duplicates
                        var friendsFriends = subscribers[friend].Friends;
                        friendsFriends.Add(id); // symmetric relation
                    }
                }
            }
        }

        /// <summary>
        /// Find potential friends (candidates) for subscriber: other subscribers with mutual friends
        /// Demonstrate MapReduce with sequential foreach
        /// </summary>
        /// <param name="id">ID of subscriber seeking friends</param>
        /// <param name="maxCandidates">Maximum number of potential friends to return</param>
        /// <returns>Sorted list of candidates as ID/count pairs, count is number of mutual friends</returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures")]
        public IDMultisetItemList PotentialFriendsSequential(SubscriberID id, int maxCandidates)
        {
            // Map
            var foafsList = new List<IDMultiset>();
            foreach (SubscriberID friend in subscribers[id].Friends)
            {
                var foafs = subscribers[friend].FriendsCopy();
                foafs.RemoveWhere(foaf => foaf == id|| subscribers[id].Friends.Contains(foaf)); // remove self, own friends
                foafsList.Add(Multiset.Create(foafs));
            }

            // Reduce
            IDMultiset candidates = new IDMultiset();
            foreach (IDMultiset foafs in foafsList)
            {
                candidates = Multiset.Union(foafs, candidates);
            }

            // Postprocess
            return Multiset.MostNumerous(candidates, maxCandidates);
        }

        /// <summary>
        /// Find potential friends, demonstrate MapReduce with Parallel.ForEach, same parameters as above.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures")]
        public IDMultisetItemList PotentialFriendsParallel(SubscriberID id, int maxCandidates)
        {
            object locker = new object();
            IDMultiset candidates = new IDMultiset();
            Parallel.ForEach(subscribers[id].Friends, // Map over friends
                () => Multiset.Create(new HashSet<SubscriberID>()), // init thread-local state localFoafs with empty Multiset
                (friend, loopState, localFoafs) =>
                {
                    var foafs = subscribers[friend].FriendsCopy();
                    foafs.RemoveWhere(foaf => foaf == id || subscribers[id].Friends.Contains(foaf)); // remove self, own friends
                    return Multiset.Union(localFoafs, Multiset.Create(foafs)); // Reduce, thread-local
                },
                localFoafs => { lock (locker) candidates = Multiset.Union(localFoafs, candidates); }); // Reduce, among threads 
            return Multiset.MostNumerous(candidates, maxCandidates); // postprocess results of Reduce
        }

        /// <summary>
        /// Find potential friends, demonstrate MapReduce with sequential LINQ, same parameters as above.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures")]
        public IDMultisetItemList PotentialFriendsLinq(SubscriberID id, int maxCandidates)
        {
            var candidates =
                subscribers[id].Friends                                   // Map
                .SelectMany(friend => subscribers[friend].Friends)
                .Where(foaf => foaf != id && !(subscribers[id].Friends.Contains(foaf))) // remove self, own friends
                .GroupBy(foaf => foaf)                               // Reduce
                .Select(foafGroup => new IDMultisetItem(foafGroup.Key, foafGroup.Count()));
            return Multiset.MostNumerous(candidates, maxCandidates); // postprocess results of Reduce
        }

        /// <summary>
        /// Find potential friends, demonstrate MapReduce with PLINQ, same parameters as above.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures")]
        public IDMultisetItemList PotentialFriendsPLinq(SubscriberID id, int maxCandidates)
        {
            var candidates =
                subscribers[id].Friends.AsParallel()                       // Map
                .SelectMany(friend => subscribers[friend].Friends)
                .Where(foaf => foaf != id && !(subscribers[id].Friends.Contains(foaf))) // remove self, own friends
                .GroupBy(foaf => foaf)                               // Reduce
                .Select(foafGroup => new IDMultisetItem(foafGroup.Key, foafGroup.Count()));
            return Multiset.MostNumerous(candidates, maxCandidates); // postprocess results of Reduce
        }

        /// <summary>
        /// Print rows subscribers from repository, up to maxFriends each
        /// </summary>
        public void Print(int rows, int maxFriends)
        {
            var subs = subscribers.GetEnumerator();
            Console.WriteLine("Subscriber    N  Friends");
            for (int i = 0; i < rows; i++)
            {
                if (subs.MoveNext()) 
                {
                    int id = subs.Current.Key;
                    Subscriber s = subs.Current.Value;
                    Console.Write("{0,10:D}", id);
                    s.Print(maxFriends); 
                }
            }
        }
    }
}