//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.SocialNetwork.Main

open System
open System.Globalization
open System.Collections.Generic
open System.Collections.ObjectModel
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.SocialNetwork
//
// Command line arguments:
//
//   Usage: SocialNework subscriberCount maxFriends
//   Both arguments are optional, defaults are 1000 and 10.
//
[<EntryPoint>]
let main (args:string[]) =
    Console.WriteLine("Social Network Sample\n")
    #if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n")
    #endif

    // Seed for random but reproducible runs
    let random = new Random(1)  

    // Defaults for printed table of results
    let maxRows = 16
    let maxCols = 8
    let maxCandidates = maxRows 

    // Defaults for data generation, may override some on command line
    let subscriberCount = 
      if args.Length > 0 then Int32.Parse(args.[0], CultureInfo.CurrentCulture) else 10000
    let maxFriends = 
      if args.Length > 1 then Int32.Parse(args.[1], CultureInfo.CurrentCulture) else 2000
    Console.WriteLine("Creating data...")

    // Allocate subscribers, assign friends for timing tests
    let subscribers = new SubscriberRepository(subscriberCount)
    subscribers.AssignRandomFriends maxFriends random

    // Print a few subscribers and a summary
    Console.WriteLine()
    Console.WriteLine("Some subscribers and some of their friends")
    subscribers.Print maxRows maxCols
    Console.WriteLine()
    Console.WriteLine("{0} subscribers in all, with up to {1} friends or even more  ", subscriberCount, maxFriends)

    // Choose a subscriber seeking friends
    let id = 0
    let subscriber = subscribers.GetSubscriber(id)
    Console.WriteLine()
    Console.WriteLine("Find potential friends for this subscriber, with these friends:")
    Console.Write("{0,10:D}", id)
    subscriber.Print(subscriber.Friends.Count)
    Console.WriteLine()

    // --------------------------------------------------------------------------
    // Run various implementations of friend search
    // --------------------------------------------------------------------------
    
    // Sequential for loop
    let candidates = 
      SampleUtilities.TimedRun "  Sequential for" (fun () ->
          subscribers.PotentialFriendsSequential id maxCandidates ) 
    Console.WriteLine()

    let rows = Math.Min(maxRows, candidates.Count)
    Console.WriteLine("{0} potential friends for this subscriber, and the number of mutual friends", rows)            
    Multiset.print candidates
    Console.WriteLine()

    // Parallel version using Parallel.ForEach 
    let candidates = 
      SampleUtilities.TimedRun "Parallel.ForEach" (fun () -> 
          subscribers.PotentialFriendsParallel id maxCandidates )
    Console.WriteLine()

    let rows = Math.Min(maxRows, candidates.Count)
    Console.WriteLine("{0} potential friends for this subscriber, and the number of mutual friends", rows)
    Multiset.print candidates
    Console.WriteLine()

    // Sequential version using LINQ
    let candidates = 
      SampleUtilities.TimedRun " Sequential LINQ" (fun () ->
          subscribers.PotentialFriendsLinq id maxCandidates )
    Console.WriteLine()

    let rows = Math.Min(maxRows, candidates.Count)
    Console.WriteLine("{0} potential friends for this subscriber, and the number of mutual friends", rows)
    Multiset.print candidates
    Console.WriteLine()

    // Parallel version using PLINQ
    let candidates = 
      SampleUtilities.TimedRun "           PLINQ" (fun () ->
          subscribers.PotentialFriendsPLinq id maxCandidates )
    Console.WriteLine()

    let rows = Math.Min(maxRows, candidates.Count)
    Console.WriteLine("{0} potential friends for this subscriber, and the number of mutual friends", rows)
    Multiset.print candidates


    // Parallel version using F# PSeq module
    let candidates = 
      SampleUtilities.TimedRun "     PSeq module" (fun () ->
          subscribers.PotentialFriendsPSeq id maxCandidates )
    Console.WriteLine()

    let rows = Math.Min(maxRows, candidates.Count)
    Console.WriteLine("{0} potential friends for this subscriber, and the number of mutual friends", rows)
    Multiset.print candidates



    Console.WriteLine("\nRun complete... press enter to finish.") 
    Console.ReadLine() |> ignore
    0