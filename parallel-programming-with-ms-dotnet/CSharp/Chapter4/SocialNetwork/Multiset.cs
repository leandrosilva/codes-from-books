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
using System.Collections.ObjectModel;

using SubscriberID = System.Int32;

namespace Microsoft.Practices.ParallelGuideSamples.SocialNetwork
{
    using IDMultiset = Dictionary<SubscriberID, int>;
    using IDMultisetItem = KeyValuePair<SubscriberID, int>;
    using IDMultisetItemList = Collection<KeyValuePair<SubscriberID, int>>;  // to sort, must use list of Multiset items not Multiset itself

    /// <summary>
    /// Multiset of IDs, represented as dictionary where Key is ID and Value is its multiplicity.
    /// </summary>
    static class Multiset
    {
        /// <summary>
        /// Initialize Multiset from HashSet, each ID has multiplicity 1
        /// </summary>
        /// <param name="friends"></param>
        /// <returns></returns>
        public static IDMultiset Create(HashSet<SubscriberID> friends)
        {
            var multiset = new IDMultiset();
            foreach (SubscriberID friend in friends)
            {
                multiset[friend] = 1;
            }
            return multiset;
        }

        /// <summary>
        /// Multiset union.
        /// </summary>
        /// <param name="multiset1">First Multiset</param>
        /// <param name="multiset2">Second Multiset</param>
        /// <returns>Union of Multiset1 and Multiset2</returns>
        public static IDMultiset Union(IDMultiset multiset1, IDMultiset multiset2)
        {
            if (multiset1.Count < multiset2.Count)
            {
                return Union(multiset2, multiset1);
            }
            else
            {
                var union = new IDMultiset();
                foreach (var item in multiset1)
                {
                    union.Add(item.Key, item.Value);
                }
                foreach (var item in multiset2)
                {
                    var friend = item.Key;
                    var count = item.Value;
                    union[friend] = union.ContainsKey(friend) ? union[friend] + count : count;
                }
                return union;
            }
        }

        /// <summary>
        /// Return sorted list of most numerous items in Multiset.
        /// </summary>
        /// <param name="multiset">Multiset of IDs</param>
        /// <param name="maxItems">Maximum number of Multiset items to return</param>
        /// <returns>List of ID/multiplicity pairs, sorted in order of decreasing multiplicity</returns>
        public static IDMultisetItemList MostNumerous(IEnumerable<IDMultisetItem> multiset, int maxItems)
        {
            // mostNumerous is sorted by item value, mostNumerous[0] is smallest 
            var mostNumerous = new IDMultisetItemList();
            foreach (IDMultisetItem item in multiset)
            {
                if (mostNumerous.Count < maxItems)
                {
                    InsertInOrder(item, mostNumerous);
                }
                else
                {
                    int lastIndex = mostNumerous.Count - 1;
                    if (item.Value > mostNumerous[lastIndex].Value) // smallest value is at the end
                    {
                        mostNumerous.RemoveAt(lastIndex);
                        InsertInOrder(item, mostNumerous);
                    }
                }
            }
            return mostNumerous;
        }

        /// <summary>
        /// Insert ID/multiplicity pair in sorted list
        /// </summary>
        /// <param name="item">ID/multiplicity pair to insert in list</param>
        /// <param name="list">list of ID/multiplicity pairs, sort by multiplicity, largest to smallest</param>
        static void InsertInOrder(IDMultisetItem item, IDMultisetItemList list)
        {
            if (list.Count == 0)
            {
                list.Insert(0, item);
            }
            else
            {
                int i;
                for (i = 0; i < list.Count; i++)
                {
                    if (item.Value >= list[i].Value) break;
                }
                list.Insert(i, item); // inserts ahead of list[i], results in descending order
            }
        }
 
        /// <summary>
        /// Print list of ID/multiplicity pairs.
        /// </summary>
        /// <param name="list">List of ID/multiplicity pairs</param>
        public static void Print(IDMultisetItemList list)
        {
            for (int i = 0; i < list.Count; i++)
            {
                Console.WriteLine("{0,10:D}{1,5:D}", list[i].Key, list[i].Value);
            }
        }
    }
}
