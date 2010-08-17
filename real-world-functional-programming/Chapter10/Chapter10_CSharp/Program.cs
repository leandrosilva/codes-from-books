using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using FunctionalCSharp;

namespace Chapter10_CSharp
{
  // --------------------------------------------------------------------------
  // Section Using arrays in a functional way in C#
  // (This is needed later, but the class cannot be nested, so we have it here...
  
  // Listing 10.13 Methods for functional array processing

  static class ArrayUtils
  {
    // Initializes array using the given function
    public static T[] Create<T>(int length, Func<int, T> f) {
      T[] arr = new T[length];
      for (int i = 0; i < length; i++) arr[i] = f(i);
      return arr;
    }

    // Extension method that returns an array
    public static R[] Select<T, R>(this T[] arr, Func<T, R> f) {
      R[] res = new R[arr.Length];
      for (int i = 0; i < arr.Length; i++) res[i] = f(arr[i]);
      return res;
    }
  }
  
	class Program
	{
	  // --------------------------------------------------------------------------
    // Section 10.1.2 Caching results using memoization

    // Returns memoized version of the 'func' function
		static Func<T, R> Memoize<T, R>(Func<T, R> func) {
      // Cache captured by the closure
      var cache = new Dictionary<T, R>();
			return arg => {
        R val;				
        if (cache.TryGetValue(arg, out val))
          // Return cached value
          return val;
        else {
          // Calculate the value and add it to the cache
          val = func(arg);
	        cache.Add(arg, val);
	        return val;
				  }
		  };
		}

    // Example showing how to use the function above 
		static void MainMemoization()
		{
      // Note: This example requires that two tuples containing
      // the same values are treated as equal. The 'Tuple' class
      // in the functional library now implements Equals & GetHashCode

			var addMem = Memoize((Tuple<int, int> arg) => {
			    Console.WriteLine("adding {0} + {1}", arg.Item1, arg.Item2);
			    return arg.Item1 + arg.Item2;
		    });

			Console.WriteLine(addMem(Tuple.Create(19, 23)));
      Console.WriteLine(addMem(Tuple.Create(19, 23)));
      Console.WriteLine(addMem(Tuple.Create(18, 24)));
    }

    // --------------------------------------------------------------------------
    // Section Using arrays in a functional way in C#

		private static void MainArrays()
		{
			Random rnd = new Random();
      
      // Fill array with random numbers
			var numbers = ArrayUtils.Create(5, n => rnd.Next(20));
      // Store the results in an anonymous type
      var squares = numbers.Select(n => new { Number = n, Square = n * n });

			foreach (var sq in squares)
				Console.Write("({0}, {1}); ", sq.Number, sq.Square);
		}

    // --------------------------------------------------------------------------
    // Section 10.3 Introducing continuations

    // Listing 10.15 Writing code using continuations
		
    // Returning the result in the standard way
    int StrLength(string s) {
			return s.Length;
		}
		void TestLength() {
			int l1 = StrLength("One");
			int l2 = StrLength("Two");
			Console.WriteLine(l1 + l2);
		}

    // Returning result by calling the given function (continuation)
		void StrLengthCont(string s, Action<int> cont) {
			cont(s.Length);
		}
		void HelloWorldLengthCont() {
			StrLengthCont("Hello", l1 => 
				StrLengthCont("World", l2 => 
          Console.WriteLine(l1 + l2)
        ));
		}

    // --------------------------------------------------------------------------
    // Entry point for the whole example - select the part you're interested in
		static void Main(string[] args)
		{
			// MainMemoization();
      // MainArrays();
		}
	}
}
