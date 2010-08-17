using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using FunctionalCSharp;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 11
// --------------------------------------------------------------------------
// This source file contains all the examples with exception of
// the larger 'photos' application that uses lazy values to cache
// resized photo previews
// --------------------------------------------------------------------------

namespace Chapter11_CSharp
{
	class Program
	{
    // --------------------------------------------------------------------------
    // Section 11.1.2 Tracking dependencies in the code

    // Listing 11.4 Working with places stored in List<T> (C#)

    // Returns list containing names
    static List<string> LoadPlaces() {
      return new List<string> { "Seattle", "Prague", 
        "New York", "Grantchester", "Cambridge" };
    }

    // Prints place with the longest name
    static void PrintLongest(List<string> names) {
		  // Start with the first place
    	string longest = names[0];    
      for(int i = 2; i < names.Count; i++)
				if (names[i].Length > longest.Length) 
          // A name is longer than the longest so far
          longest = names[i];
			Console.WriteLine(longest);
		}

    // Print the count of multi-word names 
    static void PrintMultiWord(List<string> names) {
      // Remove all single-word names
      names.RemoveAll(s => !s.Contains(" "));
      Console.WriteLine("With space: {0}", names.Count);
		}

    static void MainMutableLists() {
      // We can get different results if we do a simple optimization..
      PrintMultiWord(LoadPlaces());
      PrintLongest(LoadPlaces());   // Prints 'Grantchester'

      var places = LoadPlaces();
      PrintMultiWord(places);
      PrintLongest(places);   // Prints 'New York'
    }


    // --------------------------------------------------------------------------
    // Section Using immutable data structures

    // NOTE: The following two functions aren't included in the book text

    // Returns places as an immutable list
    static IEnumerable<string> LoadImmutablePlaces() {
      return FuncList.Cons("Cambridge", FuncList.Cons("Grantchester",
        FuncList.Cons("New York", FuncList.Cons("Prague",
          FuncList.Cons("Seattle", FuncList.Empty<string>())))));
    } 

    // Listing 11.5 Implementation of 'PrintMultiWord' using immutable list
    static IEnumerable<string> PrintMultiWord(IEnumerable<string> names) {
      var namesSpace = names.Where(s => s.Contains(" "));
      Console.WriteLine("With space: {0}", namesSpace.Count());
      return namesSpace;
    }

    // Prints place with the longest name
    static void PrintLongest(IEnumerable<string> names)
    {
      // Start with the first place
      string longest = names.First();
      foreach(string name in names)
        if (name.Length > longest.Length)
          // A name is longer than the longest so far
          longest = name;
      Console.WriteLine(longest);
    }

    static void MainImmutableLists() {
      // Listing 11.6 Printing the longest and the longest multi-word name
      IEnumerable<string> pl = LoadImmutablePlaces();
      PrintMultiWord(pl);
      // Prints 'Grantchester'
      PrintLongest(pl);

      IEnumerable<string> pl1 = LoadImmutablePlaces();
      // Returns filtered list
      var pl2 = PrintMultiWord(pl1);
      // Prints 'New York'
      PrintLongest(pl2);
    }

    // --------------------------------------------------------------------------
    // Section 11.3.3 Simulating lazy evaluation using functions

    // Listing 11.14 Lazy "or" operator using functions

    // Prints information about the call as a side-effect
    static bool Foo(int n) {
      Console.WriteLine("Foo({0})", n);
      return n <= 10;
    }

    // Takes functions instead of values
    static bool LazyOr(Func<bool> a, Func<bool> b) {
      if (a()) return true;       // Force evaluation of the first argument
      else if (b()) return true;  // Force evaluation of the second argument
      else return false;
    }

    static void MainLazyOrFunction() {
      // Prints 'Foo(5)' and 'True' only
      if (LazyOr(() => Foo(5), () => Foo(7)))
        Console.WriteLine("True"); 
    }


    // --------------------------------------------------------------------------
    // Section 11.3.4 Implementing lazy values for C#

    // Note: The implementation can be found in the 'Lazy.cs' in 'FunctionalCSharp'

		private static void MainLazyValues()
		{
      var l = Lazy.Create(() => Foo(10));
      Console.WriteLine("First:");
      Console.WriteLine(l.Value);
      Console.WriteLine("Second:");
      Console.WriteLine(l.Value);
		}

    // Note: More examples using lazy values is available in 'Photos' demo 

    // --------------------------------------------------------------------------
    // Entry point for all the examples

    static void Main(string[] args) {
      MainMutableLists();
      MainImmutableLists();
      MainLazyValues();
    }
	}
}
