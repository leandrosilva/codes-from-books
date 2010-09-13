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
open System.Linq
open System.Threading.Tasks
open System.Collections.ObjectModel
open System.Diagnostics.CodeAnalysis
open Microsoft.FSharp.Collections
open Microsoft.Practices.ParallelGuideSamples.SocialNetwork
    
/// Represents a social network that keeps a list of subscribers
/// and supports finding of potential friends (various parallel implementations)
type SubscriberRepository(subscriberCount:int) = 

    /// Collection of all subscribers in the repository
    let subscribers = new Dictionary<SubscriberID, Subscriber>()
    
    do  /// Constructor, allocate subscribers with no friends
        for subscriber in 0 .. subscriberCount - 1 do
            subscribers.[subscriber] <- new Subscriber()

    /// Constructor, initialize with no subscribers
    new() = SubscriberRepository(0)

    /// Return number of subscribers
    member x.Count = subscribers.Count

    /// Return subscriber with given ID
    member x.GetSubscriber subId = subscribers.[subId]

    /// Assign every subscriber a random number (up to maxFriends) of randomly 
    /// chosen new friends. Ensure friends relation is symmetric.
    member x.AssignRandomFriends maxFriends (random:Random) =
        for (KeyValue(subId, subscriber)) in subscribers do
            if random = null then nullArg "random"
            let nfriends = random.Next(maxFriends)
            let friends = subscriber.Friends
            for i in 0 .. nfriends do
                let friend = random.Next(subscribers.Count)
                if friend <> subId then
                    friends.Add(friend) |> ignore // HashSet ensures no duplicates
                    let friendsFriends = subscribers.[friend].Friends
                    friendsFriends.Add(subId) |> ignore // symmetric relation


    /// Print rows subscribers from repository, up to maxFriends each
    member x.Print rows maxFriends =
        Console.WriteLine("Subscriber    N  Friends")
        subscribers
          |> Seq.take (min rows subscribers.Count)
          |> Seq.iter (fun (KeyValue(subId, s)) ->
                Console.Write("{0,10:D}", subId)
                s.Print(maxFriends) )

    // --------------------------------------------------------------------------
    // Various implementations of friend suggestions
    // --------------------------------------------------------------------------

    /// <summary>
    ///   Find potential friends (candidates) for subscriber: other subscribers with 
    ///   mutual friends. Demonstrate MapReduce with sequential foreach.
    /// </summary>
    /// <param name="id">ID of subscriber seeking friends</param>
    /// <param name="maxCandidates">Maximum number of potential friends to return</param>
    /// <returns>Sorted list of candidates as ID/count 
    ///   pairs, count is number of mutual friends</returns>
    [<SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures")>]
    member x.PotentialFriendsSequential subId maxCandidates =
        // Map phase
        let foafsList = 
          subscribers.[subId].Friends 
            |> Seq.map (fun friend ->
              let foafs = subscribers.[friend].FriendsCopy()
              foafs.RemoveWhere(fun foaf -> 
                  // Remove self, own friends
                  foaf = subId || subscribers.[subId].Friends.Contains(foaf)) |> ignore
              Multiset.create foafs )
            |> Array.ofSeq

        // Reduce phase
        foafsList 
          |> Seq.fold Multiset.union (new IDMultiset())
          |> Multiset.mostNumerous maxCandidates

    /// Find potential friends, demonstrate MapReduce with 
    /// Parallel.ForEach, same parameters as above.
    [<SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures")>]
    member x.PotentialFriendsParallel subId maxCandidates = 
        let locker = new obj()
        let candidates = ref (new IDMultiset())

        Parallel.ForEach
          ( // Map over friends
            subscribers.[subId].Friends, 
            // Init thread-local state localFoafs with empty Multiset
            (fun () -> Multiset.create (new HashSet<SubscriberID>())),
            (fun friend loopState localFoafs ->
                let foafs = subscribers.[friend].FriendsCopy()
                foafs.RemoveWhere(fun foaf -> 
                    // Remove self, own friends
                    foaf = subId || subscribers.[subId].Friends.Contains(foaf)) |> ignore
                // Reduce, thread-local
                Multiset.union localFoafs (Multiset.create foafs) ),
            // Reduce, among threads 
            (fun localFoafs ->  
                lock locker (fun () -> 
                    candidates := Multiset.union localFoafs !candidates))) |> ignore

        // Postprocess results of Reduce
        Multiset.mostNumerous maxCandidates (!candidates) 


    /// Find potential friends, demonstrate MapReduce with 
    /// sequential LINQ, same parameters as above.
    [<SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures")>]
    member x.PotentialFriendsLinq subId maxCandidates =
        let candidates =
          subscribers.[subId].Friends
            // Map phase
            .SelectMany(fun friend -> subscribers.[friend].Friends :> seq<_>)
            .Where(fun foaf -> 
                // remove self, own friends
                foaf <> subId && not (subscribers.[subId].Friends.Contains(foaf))) 
            // Reduce phase
            .GroupBy(fun g -> g)
            .Select(fun (foafGroup:IGrouping<_, _>) -> 
                new IDMultisetItem(foafGroup.Key, foafGroup.Count()))
        
        // Postprocess results of Reduce
        Multiset.mostNumerous maxCandidates candidates


    /// Find potential friends, demonstrate MapReduce with 
    /// PLINQ, same parameters as above.
    [<SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures")>]
    member x.PotentialFriendsPLinq subId maxCandidates =
        let candidates =
          subscribers.[subId].Friends
            .AsParallel()
            // Map phase
            .SelectMany(fun friend -> subscribers.[friend].Friends :> seq<_>)
            .Where(fun foaf -> 
                // remove self, own friends
                foaf <> subId && not (subscribers.[subId].Friends.Contains(foaf))) 
            // Reduce phase
            .GroupBy(fun g -> g)
            .Select(fun (foafGroup:IGrouping<_, _>) -> 
                new IDMultisetItem(foafGroup.Key, foafGroup.Count()))
        
        // Postprocess results of Reduce
        Multiset.mostNumerous maxCandidates candidates 

        
    /// Find potential friends, demonstrate MapReduce with 
    /// F# PSeq module, same parameters as above.
    [<SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures")>]
    member x.PotentialFriendsPSeq subId maxCandidates =
        let candidates =
          subscribers.[subId].Friends
            |> PSeq.collect (fun friend -> subscribers.[friend].Friends)
            |> PSeq.filter (fun foaf ->
                // remove self, own friends
                foaf <> subId && not (subscribers.[subId].Friends.Contains(foaf)))
            // Reduce phase
            |> PSeq.groupBy id
            |> PSeq.map (fun (k, v) -> new IDMultisetItem(k, v.Count()))
        
        // Postprocess results of Reduce
        Multiset.mostNumerous maxCandidates candidates 
