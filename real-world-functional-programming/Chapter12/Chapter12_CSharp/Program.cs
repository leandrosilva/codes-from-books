// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 12
// --------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using FunctionalCSharp;
using System.Windows.Forms;
using System.Drawing;

namespace Chapter12_CSharp
{
	class Program
  {
    #region Entry point

    static void Main(string[] args)
    {
      DemoFactorials();
      DemoColors();
      DemoSelectMany();
      DemoQueries();
      DemoValueWrapper();
    }

    #endregion

    // --------------------------------------------------------------------------
    // Section 12.1 Generating sequences

    static IEnumerable<string> Factorials() {
      // Local mutable state
      int factorial = 1;
      for (int num = 0; factorial < 1000000; num++) {
        // Return the next string
        yield return String.Format("{0}! = {1}", num, factorial);
        // Modify the state of the iterator
        factorial = factorial * (num + 1);
      }
    }

    // Runs the example
    private static void DemoFactorials() {
      foreach (var s in Factorials())
        Console.WriteLine(s);
    }

    // ----------------------------------------------------------------------------
    // Section 12.2.2 Using infinite sequences 
		
    static IEnumerable<int> Numbers { get { 
			return new int [] { 490, 485, 450, 425, 365, 340, 290, 230, 130, 90, 70 };
		} }

    // Listing 12.5 Generating infinite sequence of random colors

    static Random rnd = new Random();

    static IEnumerable<Color> ColorsRandom() {
			while(true) {
				yield return Color.FromArgb(rnd.Next(256), rnd.Next(256), rnd.Next(256));
			}
		}

    // Listing 12.7 Generating a sequence with color gradients

		static IEnumerable<Color> ColorsGreenBlack() {
			while (true) {
				for (int g = 0; g < 255; g += 25) {
					int r = g / 2, b = g / 3;
					yield return Color.
						 FromArgb(r, g, b);
				}
			}
		}

    // Runs the example
		static void DemoColors() {
      // Combine data with colors into one sequence
      var clrData = Seq.Zip(Numbers, ColorsGreenBlack());
			
      var frm = new Form() { ClientSize = new Size(500,400) };
			frm.Paint += (sender, e) => {
		    e.Graphics.FillRectangle(Brushes.White, 0, 0, 500, 400);
        
        // Iterate over data and colors with index
        int i = 0;
				foreach(var itm in clrData) {
			     using(var br = new SolidBrush(itm.Item2))
             // Calculate location of the bar using index
             e.Graphics.FillRectangle(br, 0, i++ * 33, itm.Item1, 30);
				}
			};
		  Application.Run(frm);
		}

    // ----------------------------------------------------------------------------
    // Section 12.3.3 Flattening projections
    
    class CityInfo {
      public string City { get; set; }
      public string Country { get; set; }
    }

    private static void DemoSelectMany() {
			var cities = new List<CityInfo> { 
					new CityInfo { City = "London", Country = "UK" },
					new CityInfo { City = "Cambridge", Country = "UK" },
					new CityInfo { City = "Cambridge", Country = "USA" },
					new CityInfo { City = "New York", Country = "USA" } 
				};
			var entered = new List<string> { "London", "Cambridge" };

      // Listing 12.13 Searching for country of entered cities using a query

			var q1 =
        // Iterate over the entered names
				from e in entered
        // Search all known cities
        from known in cities
        // Filter matching cities and format the output
        where known.City == e
				select string.Format("{0} from {1}", known.City, known.Country);


      // ----------------------------------------------------------------------------
      // Listing 12.14 Query translated to explicit operator calls

			var q2 =
  			// Foreach entered city, iterate over known cities
      	entered
          // Create temporary value storing the results of join
					.SelectMany(e => cities, (e, known) => new { e = e, known = known })
          // Filter and format the output
          .Where(a => a.known.City == a.e)
					.Select(a => string.Format("{0} from {1}", a.known.City, a.known.Country));

			foreach (string s in q2)
				Console.WriteLine(s);
		}

    // ----------------------------------------------------------------------------
    // Section 12.4 Introducing computation expressions

    static Option<int> TryReadInt() {
      int i;
      Console.Write("Please enter a number:\n> ");
      if (Int32.TryParse(Console.ReadLine(), out i)) return Option.Some(i);
      else return Option.None<int>();
    }

    static ValueWrapper<int> ReadInt() {
      Console.Write("Please enter a number:\n> ");
      int num = Int32.Parse(Console.ReadLine());
      return new ValueWrapper<int>(num);
    }

    // Using the 'ValueWrapper' computation

    private static void DemoValueWrapper()
    {
      var v =
        from n in ReadInt()
        from m in ReadInt()
        let added = n + m
        let subtracted = n - m
        select added * subtracted;
      Console.WriteLine(v.Value);
    }

    // Working with lists & options

		private static void DemoQueries()
		{
			var numbers1 = new int[] { 2, 3 };
      var numbers2 = new int[] { 10, 100 };

      // Listing 12.15 Using queries with lists and option values

      // Working with sequences
      var list = 
        from n in numbers1
				from m in numbers2
				select n * m;

      // Write the result
      foreach (var v in list) 
        Console.Write("{0}, ", v);

      // Working with options
      var option =
          from n in TryReadInt()
          from m in TryReadInt()
          select n * m;

      // Write the result
			int str;
			if (!option.MatchSome(out str)) Console.Write("None");
			else Console.WriteLine("Some: {0}", str);
		}
	}


  // --------------------------------------------------------------------------
  // Section 12.3 Processing sequences

	class Seq 
  {
    IEnumerable<int> Squares(IEnumerable<int> numbers) {
      foreach (int i in numbers)
        yield return i * i;
    }

    // Listing 12.8 Implementing the 'Zip' method

		public static IEnumerable<Tuple<T1, T2>> Zip<T1, T2>
        (IEnumerable<T1> first, IEnumerable<T2> second) {
			// Get enumerators for both of the sequences
      using(var firstEn = first.GetEnumerator())
			using(var secondEn = second.GetEnumerator())
        // Loop until one sequence ends
			  while (firstEn.MoveNext() && secondEn.MoveNext())
          // Return elements from both sequences in a tuple 
          yield return Tuple.Create(firstEn.Current, secondEn.Current);
		}
	}

  // ----------------------------------------------------------------------------
  // Section 12.5 First steps in custom computations

  // Listing 12.17 Value of the computation
  class ValueWrapper<T> {
    public ValueWrapper(T value) {
      this.Value = value;
    }
		public T Value { get; private set; }
	}

  // Listing 12.20 Implementing query operators

  static class Values {
		public static ValueWrapper<TResult> Select<TSource, TResult>
        (this ValueWrapper<TSource> source, Func<TSource, TResult> sel) {
			return new ValueWrapper<TResult>(sel(source.Value));
		}

		public static ValueWrapper<TResult> SelectMany<TSource, TValue, TResult>
        ( this ValueWrapper<TSource> source, 
          Func<TSource, ValueWrapper<TValue>> valueSelector, 
          Func<TSource, TValue, TResult> resultSelector) {
			var value0 = valueSelector(source.Value);
			return new ValueWrapper<TResult>(resultSelector(source.Value, value0.Value));
		}
	}

  // ----------------------------------------------------------------------------
  // Section 12.6 Implementing computation expressions for options

  static class OptionLinq
  {
    // Listing 12.22 Query operators for option type
    public static Option<R> Select<S, R>(this Option<S> source, Func<S, R> sel) {
      // Same operation is called 'Map'
      return source.Map(sel);
    }

    public static Option<R> SelectMany<S, C, R>(this Option<S> source, Func<S, Option<C>> sel, Func<S, C, R> selRes) {
      // Use 'Bind', which we have already
      return source.Bind(s => 
        // Format the result
        sel(s).Map(c => selRes(s, c)));
    }
  }

}
