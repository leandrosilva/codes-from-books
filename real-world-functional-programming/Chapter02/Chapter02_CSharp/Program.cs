using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Chapter02_CSharp
{
	// --------------------------------------------------------------------------
	// Functional Programming in .NET - Chapter 2
	// --------------------------------------------------------------------------
  
	class Program
	{
    // ------------------------------------------------------------------------
    // Section 2.2.1 Working with immutable values

    #region Required utilit methods

    static int GetInitialValue() { return 0; }
    static int ReadInt32() { return Int32.Parse(Console.ReadLine()); }
    static void WriteInt32(int i) { Console.WriteLine(i); }

    #endregion 

    static void CalculationImperative() 
    {
      int res = GetInitialValue();
      res = res + ReadInt32();
      res = res * ReadInt32();
      WriteInt32(res);
    }

    static void CalculationFunctional()
    {
      // In this version of the code, we don't modify a value
      // of a variable once it is defined.
      int res0 = GetInitialValue();
      int res1 = res0 + ReadInt32();
      int res2 = res1 * ReadInt32();
      WriteInt32(res2);
    }

		// ------------------------------------------------------------------------
		// Section 2.2.3: Changing program state using recursion

		// Imperative implementation
		int SumNumbersImperative(int from, int to)
		{
			int res = 0;
			for (int i = from; i <= to; i++)
				res = res + i;
			return res;
		}

		// Functional implementation using recursion
		int SumNumbersFunctional(int from, int to)
		{
			if (from > to) return 0;
			int sumRest = SumNumbers(from + 1, to);
			return from + sumRest;
		}


    // ------------------------------------------------------------------------
    // Section 2.2.4 How is the calculation written?

    // Listing 2.1 Summing numbers in the specified range in a "functional C#"
    int SumNumbers(int from, int to) {
      // The method body contains only 'return' 
      return
        (from > to)
         // Value for the 'then' case
         ? 0  
         // NOTE: for demonstration purposes, we use a 
         // pseudo-C# 'var' declaration in the book here
         
         // Expression calculating the 'else' case
         : (from + SumNumbers(from + 1, to)); 
    }


		// ------------------------------------------------------------------------
		// Section 2.3.1: Functions as values

    int AggregateNumbers(Func<int, int, int> op, int init, int from, int to) {
      if (from > to) return init;
      int sumRest = AggregateNumbers(op, init, from + 1, to);
      return op(from, sumRest);
    }

    #region Application entry-point

    static void Main()
    {
    }

    #endregion
  }
}
