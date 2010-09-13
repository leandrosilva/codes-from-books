//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.SocialNetwork

open System
open System.Collections.Generic
open System.Collections.ObjectModel

type SubscriberID = int

type IDMultiset = Dictionary<SubscriberID, int>
type IDMultisetItem = KeyValuePair<SubscriberID, int>
type IDMultisetItemList = Collection<KeyValuePair<SubscriberID, int>>  // to sort, must use list of Multiset items not Multiset itself

/// Multiset of IDs, represented as dictionary where Key is ID and Value is its multiplicity.
module Multiset =

    /// Initialize Multiset from HashSet, each ID has multiplicity 1
    let create (friends:HashSet<SubscriberID>) =
        let multiset = new IDMultiset()
        for friend in friends do
            multiset.[friend] <- 1
        multiset

    /// Union of two multisets given as arguments
    let rec union (multiset1:IDMultiset) (multiset2:IDMultiset) =
        if multiset1.Count < multiset2.Count then
            union multiset2 multiset1
        else
            let union = new IDMultiset()
            for item in multiset1 do
                union.Add(item.Key, item.Value)
            for (KeyValue(friend, count)) in multiset2 do
                union.[friend] <- if union.ContainsKey(friend) then union.[friend] + count else count
            union

    /// <summary>
    /// Insert ID/multiplicity pair in sorted list
    /// </summary>
    /// <param name="item">ID/multiplicity pair to insert in list</param>
    /// <param name="list">list of ID/multiplicity pairs, sort by multiplicity, largest to smallest</param>
    let insertInOrder item (list:IDMultisetItemList) =
        if list.Count = 0 then
            list.Insert(0, item)
        else
            let i = list |> Seq.tryFindIndex (fun l -> item.Value >= l.Value)
            let i = defaultArg i list.Count
            // inserts ahead of list[i], results in descending order
            list.Insert(i, item)


    /// <summary>
    /// Return sorted list of most numerous items in Multiset.
    /// </summary>
    /// <param name="multiset">Multiset of IDs</param>
    /// <param name="maxItems">Maximum number of Multiset items to return</param>
    /// <returns>List of ID/multiplicity pairs, sorted in order of decreasing multiplicity</returns>
    let mostNumerous maxItems (multiset:IEnumerable<IDMultisetItem>) = 
        // mostNumerous is sorted by item value, mostNumerous[0] is smallest 
        let mostNumerous = new IDMultisetItemList()
        for item in multiset do
            if mostNumerous.Count < maxItems then
                insertInOrder item mostNumerous
            else
                let lastIndex = mostNumerous.Count - 1
                if item.Value > mostNumerous.[lastIndex].Value then
                    // smallest value is at the end
                    mostNumerous.RemoveAt(lastIndex)
                    insertInOrder item mostNumerous
        mostNumerous

 
    /// Print list of ID/multiplicity pairs.
    let print (list:IDMultisetItemList) =
        for (KeyValue(k,v)) in list do
            Console.WriteLine("{0,10:D}{1,5:D}", k, v)
