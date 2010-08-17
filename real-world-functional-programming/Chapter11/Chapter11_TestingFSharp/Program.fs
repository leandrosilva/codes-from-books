// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 11
// --------------------------------------------------------------------------
// This source file contains all the examples related to unit-testing
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 11.2.1 From the interactive shell to unit tests

// Listing 11.7 Testing code interactively using xUnit.net
#light
#if INTERACTIVE
#r @"C:\Programs\Development\xUnit\xunit.dll"
#endif

open Xunit

#if INTERACTIVE
let test = [ "Aaa"; "Bbbbb"; "Cccc" ]
Assert.Equal("Bbbbb", getLongest(test))
#endif

// Listing 11.8 Function with unit tests to verify its behavior

let getLongest(names:list<string>) =
  match names with
  | [] -> ""
  | _ -> 
    names |> List.maxBy (fun name -> name.Length)
 
module Tests = 
  // Mark tests using 'Fact' attribute

  // Adjusted interactive test
  [<Fact>]
  let longestOfNonEmpty() = 
    let test = [ "Aaa"; "Bbbbb"; "Cccc" ]
    Assert.Equal("Bbbbb", getLongest(test))    

  // Should return first of the longest elements
  [<Fact>]
  let longestFirstLongest() = 
    let test = [ "Aaa"; "Bbb" ]
    Assert.Equal("Aaa", getLongest(test))    

  // Should return empty string for an empty list
  [<Fact>]
  let longestOfEmpty() = 
    let test = []
    Assert.Equal("", getLongest(test))    

// --------------------------------------------------------------------------
// Section 11.2.2 Writing tests using structural equality

// Listing 11.9 Comparing records with structural equality

// Create records containing the same values
type WeatherItem = 
  { Temperature : int * int
    Text : string }
    
let r1 = { Temperature = -10, -2; Text = "Winter" }
let r2 = { Temperature = -10, -2; Text = "Winter" }

#if INTERACTIVE
// Values are represented by different instances
System.Object.ReferenceEquals(r1, r2)

// …but are considered as equal
r1.Equals(r2)
r1 = r2

let r3 = { Temperature = 10, 20; Text = "Summer" };;
r3 > r1
#endif

// --------------------------------------------------------------------------
// Listing 11.10 Unit tests for the partitioning operation 

let partitionMultiWord(names:list<string>) =
  names |> List.partition (fun name -> name.Contains(" "))
  
module Tests2 = 
  
  // Verify length of the returned lists
  [<Fact>]
  let partitionKeepLength() = 
    let test = ["A"; "A B"; "A B C"; "B" ]
    let multi, single = partitionMultiWord(test)
    Assert.True(multi.Length + single.Length = test.Length)

  // Test the result using structural equality
  [<Fact>]
  let partitionNonEmpty() = 
    let test = ["Seattle"; "New York"]
    let expected = ["New York"], ["Seattle"]
    Assert.Equal(expected, partitionMultiWord(test))
  
  [<Fact>]
  let partitionThanLongest() = 
    let test = ["Seattle"; "New York"; "Grantchester"]
    let expected = ["New York"], ["Seattle"; "Grantchester"]

    // Run the functions in sequence
    let actualPartition = partitionMultiWord(test)
    let actualLongest = getLongest(test)
    
    // Verify the results
    Assert.Equal(expected, actualPartition)
    Assert.Equal("Grantchester", getLongest(test))
  
//getLongest(null)
//partitionMultiWord([])

