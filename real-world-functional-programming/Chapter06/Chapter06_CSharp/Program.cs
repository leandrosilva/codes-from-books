using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using FunctionalCSharp;

namespace Chapter06_CSharp
{
	// --------------------------------------------------------------------------
	// Functional Programming in .NET - Chapter 6
	// --------------------------------------------------------------------------
	// NOTE: In this chapter we added some functionality to the 
	// 'FunctionalCSharp' library as well, so you can find some examples there.
	// --------------------------------------------------------------------------

	static class Program
	{
		#region Program entry-point

		static void Main(string[] args)
		{
			Program.MainExtensions();
			Program.MainTuples();
			Program.MainFunctions();
			Program.MainLists();
		}

		#endregion

		// ------------------------------------------------------------------------
		// Listing 6.3: Workign with strings using extension methods

		// Static method with 'this' keyword before the first parameter
		public static string AddLine(this string str, string next)
		{
			return str + "\n>>" + next;
		}

		// Concatenate strings using extension method
		static void MainExtensions()
		{
			Console.WriteLine("Hello world!".AddLine
											 ("How are you today").AddLine
											 ("I'm fine!"));
		}

		// ------------------------------------------------------------------------
		// Section 6.2: Working with tuples

		static void MainTuples()
		{
			// Section 6.2.1: Methods for working with tuples in C#

			var oldPrague = Tuple.Create("Prague", 1188000);
			// Direct call to the static method
			var newPrague1 = TupleExtensions.MapSecond(oldPrague, n => n + 13195);
			// Calling using extension method
			var newPrague2 = oldPrague.MapSecond(n => n + 13195);
		}

		// ------------------------------------------------------------------------
		// Section 6.3.1: Calculating with schedules

    #region Class hierarchy for representing schedule (from chapter 5)

    enum ScheduleType { Never, Repeatedly, Once };

    abstract class Schedule
    {
      public ScheduleType Tag { get; set; }
    }

    class Never : Schedule
    { }

    class Repeatedly : Schedule
    {
      public Repeatedly(DateTime first, TimeSpan periodicity)
      {
        StartDate = first;
        Interval = periodicity;
      }
      public DateTime StartDate { get; private set; }
      public TimeSpan Interval { get; private set; }
    }

    class Once : Schedule
    {
      public Once(DateTime when) { EventDate = when; }
      public DateTime EventDate { get; private set; }
    }

    #endregion

		// Listing 6.9 Map operation for schedule type

		// Extension method for the 'Schedule' values
		static Schedule MapSchedule(this Schedule schedule, Func<DateTime, DateTime> calcDate)
		{
			switch (schedule.Tag)
			{
				case ScheduleType.Never:
					return new Never();
				case ScheduleType.Once:
					// Calculate new date
					var os = (Once)schedule;
					return new Once(calcDate(os.EventDate));
				case ScheduleType.Repeatedly:
					// Calculate new date for the first occurrence
					var rs = (Repeatedly)schedule;
					return new Repeatedly(calcDate(rs.StartDate), rs.Interval);
			}
			// No other option; shouldn't happen!
			throw new InvalidOperationException();
		}

		// ------------------------------------------------------------------------
		// Section 6.3.2: Working with option type

		#region 'ReadInput' function from chapter 5

		static Option<int> ReadInput()
		{
			int num;
			if (Int32.TryParse(Console.ReadLine(), out num)) return Option.Some(num);
			else return Option.None<int>();
		}

		#endregion

		static Option<int> ReadAndAdd()
		{
			return ReadInput().Bind(n =>
					ReadInput().Map(m => m + n));
		}

		// ------------------------------------------------------------------------
		// Section 6.4.1: Function composition

		// Listing 6.15 Implementing and using Compose method (C#)

		// Function composition returns a function value
		static Func<A, C> Compose<A, B, C>(this Func<A, B> f, Func<B, C> g)
		{
			// Construct the composed function using lambda function
			return (x) => g(f(x));
		}

		static void MainFunctions()
		{
			// Using function composition in C#
			Func<double, double> square = (n) => n * n;
			Func<double, string> fmtnum = (n) => n.ToString("E");

			// Compose two functions
			var data = new double[] { 1.1, 2.2, 3.3 };
			var sqrs = data.Select(square.Compose(fmtnum));

			// Prints: "1.210000E+000"; "4.840000E+000"; "1.089000E+001"
			foreach (var s in sqrs) Console.Write(s);
		}

		// ------------------------------------------------------------------------
		// Section 6.6: Working with lists


		// Listing 6.19: Data about settlements 

		// Class that represents information about cities
		class CityInfo
		{
			public string Name { get; set; }
			public int Population { get; set; }
		}

		static void MainLists()
		{
			// Create a collection with information for testing
			var places = new List<CityInfo>() {
				new CityInfo { Name="Seattle", Population=594210 },
				new CityInfo { Name="Prague", Population=1188126 }, 
				new CityInfo { Name="New York", Population=7180000 },
				new CityInfo { Name="Grantchester", Population=552 }, 
				new CityInfo { Name="Cambridge", Population=117900 } };

			// Filter large cities and select only their name
			var names = places
				.Where(ci => ci.Population > 1000000)
				.Select(ci => ci.Name);

			// Subsection: Implementing and using Fold in C#

			var res = 
				places.Aggregate(new { First = true, Result = "" }, (r, pl) => {
					var n = r.First ? pl.Name.PadRight(20) : (pl.Name + "\n");
					return new { First = !r.First, Result = r.Result + n };
				});
			
			Console.WriteLine(res.Result);
		}


		// Listing 6.22: Functional and imperative implementation of FoldLeft

		// Functional implementation of FoldLeft for 'FuncList' class
		static R FoldLeft<T, R>(this FuncList<T> ls, Func<R, T, R> f, R init)
		{
			if (ls.IsEmpty) 
				return init;
			else 
				return ls.Tail.FoldLeft(f, f(init, ls.Head));
		}

		// Imperative implementation of FoldLeft for generic .NET List
		static R FoldLeft<T, R>(this List<T> ls, Func<R, T, R> f, R init)
		{
			R temp = init;
			foreach (var el in ls)
				temp = f(temp, el);
			return temp;
		}
	}
}