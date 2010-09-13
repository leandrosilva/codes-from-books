//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

#r @"..\..\Utilities\bin\Release\Utilities.dll"
#load @"..\..\PSeq.fs"

#load "Multiset.fs"
#load "Subscriber.fs"
#load "SubscriberRepository.fs"

open System
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.SocialNetwork



// Seed for random but reproducible runs
let random = new Random(1)  

// Defaults for printed table of results
let maxRows = 16
let maxCols = 8
let maxCandidates = maxRows 

// Defaults for data generation, may override some on command line
let subscriberCount = 10000
let maxFriends = 2000

// Allocate subscribers, assign friends for timing tests
let subscribers = new SubscriberRepository(subscriberCount)
subscribers.AssignRandomFriends maxFriends random

// Print a few subscribers and a summary
subscribers.Print maxRows maxCols

// Choose a subscriber seeking friends
let idSub = 0
let subscriber = subscribers.GetSubscriber(idSub)
// Print friends of this subscriber
subscriber.Print(subscriber.Friends.Count)

// --------------------------------------------------------------------------
// Run various implementations of friend search
// --------------------------------------------------------------------------
    
#time "on"

do 
    // *** TODO ***:
    // Run one version of the algorithm here

    // Sequential for loop
    let candidates = subscribers.PotentialFriendsSequential idSub maxCandidates
    // Parallel version using Parallel.ForEach 
    let candidates = subscribers.PotentialFriendsParallel idSub maxCandidates
    // Sequential version using LINQ
    let candidates = subscribers.PotentialFriendsLinq idSub maxCandidates
    // Parallel version using PLINQ
    let candidates = subscribers.PotentialFriendsPLinq idSub maxCandidates
    // Parallel version using F# PSeq module
    let candidates = subscribers.PotentialFriendsPSeq idSub maxCandidates


    // Print the list of found candidates for friendship
    Multiset.print candidates

#time "off"