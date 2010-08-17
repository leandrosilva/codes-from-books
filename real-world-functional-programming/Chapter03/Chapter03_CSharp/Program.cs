using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using FunctionalCSharp;

namespace Chapter03_CSharp
{
	// --------------------------------------------------------------------------
	// Functional Programming in .NET - Chapter 3
	// --------------------------------------------------------------------------
	// NOTE: In this chapter we implement several classes that we'll need later
	// in the book, so classes like Tuple etc. can be found in the 
	// 'FunctionalCSharp' library.
	// --------------------------------------------------------------------------

	class Program
	{
		#region Program entry-point
		
		static void Main(string[] args)
		{
			Program.TuplesMain1();
			Program.TuplesMain2();
			Program.ListsMain();
			Program.AggregateMain();
		}

		#endregion

		// ------------------------------------------------------------------------
		// Section 3.2.1: Introducing tuple type

		// Listing 3.6: Working with tuples in C#

		// Function that prints information about the city
		static void PrintCity(Tuple<string, int> cityInfo)
		{
			Console.WriteLine("Population of {0} is {1}.",
					cityInfo.Item1, cityInfo.Item2);
		}

		static void TuplesMain1()
		{
			// Create tuples representing Prague and Seattle
			var prague = new Tuple<string, int>("Prague", 1188000);
			var seattle = new Tuple<string, int>("Seattle", 582000);

			// Print information about the cities
			PrintCity(prague);
			PrintCity(seattle);
		}

		static void TuplesMain2()
		{
			// Listing 3.8 Improved type inference for tuples in C#
			var prague = Tuple.Create("Prague", 1188000);
			var seattle = Tuple.Create("Seattle", 582000);

			// Listing 3.9: Incrementing population of a city in C#
			var prague0 = Tuple.Create("Prague", 1188000);
			var prague1 = prague0.WithItem2(prague0.Item2 + 13195);
			PrintCity(prague1);
		}

		// ----------------------------------------------------------------------
		// Section 3.3.1: Recursive computations

		// Listing 3.12 Factorial in both C# and F#

		// Declaration of recursive function or method
		int Factorial(int n)
		{
			if (n <= 1)
				// A case which terminates the recursion and returns 1 immediately
				return 1;
			else
				// A case which performs the recursive call to a 'Factorial' method
				return n * Factorial(n - 1);
		}

		// ------------------------------------------------------------------------
		// Section 3.3.4: Functional list processing

		// Listing 3.15 Sum of list elements (C#)
		static int SumList(FuncList<int> numbers)
		{
			if (numbers.IsEmpty)
				// Sum of empty list is 0
				return 0;
			else
				// A branch for a cons cell 
				return numbers.Head + SumList(numbers.Tail);
		}

		static void ListsMain()
		{
			// Create a list storing 1,2,3,4,5
			var list = FuncList.Cons(1, FuncList.Cons(2, FuncList.Cons(3,
									 FuncList.Cons(4, FuncList.Cons(5, FuncList.Empty<int>())))));

			// Calculates and prints "15"
			int sum = SumList(list);
			Console.WriteLine(sum);
		}

		// ------------------------------------------------------------------------
		// Section 3.4.1 Processing lists of numbers

		int MultiplyList(FuncList<int> numbers)
		{
			if (numbers.IsEmpty) return 1;
			else return numbers.Head * MultiplyList(numbers.Tail);
		}

		// Listing 3.17 Adding and multiplying list elements (C#)

    static int AggregateList(FuncList<int> list, int init, Func<int, int, int> op)
		{
			if (list.IsEmpty)
				return init;  // Return initial value for empty list
			else
			{
				// Branch for a non-empty list 
        int rest = AggregateList(list.Tail, init, op);
				return op(rest, list.Head);
			}
		}

		// Methods for testing 'AggregateList'
		static int Add(int a, int b) { return a + b; }
		static int Mul(int a, int b) { return a * b; }

		static void AggregateMain()
		{
			// Initialize a sample list 
			var list = FuncList.Cons(1, FuncList.Cons(2, FuncList.Cons(3,
									 FuncList.Cons(4, FuncList.Cons(5, FuncList.Empty<int>())))));

			// Multiply and sum elements
			//   the first call prints 15, the second 120
			Console.WriteLine(AggregateList(list, 0, Add));
			Console.WriteLine(AggregateList(list, 1, Mul));
		}
	}
}