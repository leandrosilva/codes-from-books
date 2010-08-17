using System;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using FunctionalCSharp;
using System.Collections.Generic;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 14
// --------------------------------------------------------------------------

namespace Simulation_CSharp {
  // --------------------------------------------------------------------------
  // Utilities & extensions
  
  class Utils {
    // Measure & print the time of evaluating the function
    public static T MeasureTime<T>(string msg, Func<T> f) {
      var stop = new System.Diagnostics.Stopwatch();
      stop.Start();
      var res = f();
      Console.WriteLine("- {0}ms ({1})", stop.ElapsedMilliseconds, msg);
      return res;
    }
  }

  static class Enumerable
  {
    // Reimplementation of the 'maxBy' F# function
    // Selects the element for which the given selector returns the largest number
    public static TSource MaxBy<TSource>(this IEnumerable<TSource> source, Func<TSource, double> selector) {
      return source.Skip(1).Aggregate(Tuple.Create(source.First(), selector(source.First())), (prev, current) => {
        var curr = selector(current);
        return prev.Item2 < curr ? Tuple.Create(current, curr) : prev;
      }).Item1;
    }

    // Reimplementation of the 'minBy' F# function
    // Selects the element for which the given selector returns the smallest number
    public static TSource MinBy<TSource>(this IEnumerable<TSource> source, Func<TSource, double> selector)
    {
      return source.Skip(1).Aggregate(Tuple.Create(source.First(), selector(source.First())), (prev, current) => {
        var curr = selector(current);
        return prev.Item2 > curr ? Tuple.Create(current, curr) : prev;
      }).Item1;
    }
  }

  // --------------------------------------------------------------------------
  // Section 14.3.1 Accessing shared objects safely

  // Random is not thread-safe, so this is a safe generator        
  static class SafeRandom {
    private static Random rnd = new Random();
    public static Random New() {
      // Thread-safe way to generate a new random generator
      // that should be used only by the calling thread
      lock (rnd)
        return new Random(rnd.Next());
    }
  }

  // Listing 14.22 Representing state of the world (C#)

  // Value type representing location
  public struct Location {
    public double X { get; private set; }
    public double Y { get; private set; }
    public Location(double x, double y) : this() {
      X = x; Y = y;
    }
    // Add X and Y coordinates
    public static Location operator +(Location l1, Location l2) {
      return new Location(l1.X + l2.X, l1.Y + l2.Y);
    }
    // Multiply by coefficient of type double
    public static Location operator *(Location l, double f) {
      return new Location(l.X * f, l.Y * f);
    }
    // Implemented using + and *
    public static Location operator -(Location l1, Location l2) {
      return l1 + (l2 * -1.0);
    }
  }

  // Represents the state of the simulation
  public class Simulation
  {
    // Store the collection as immutable 'IEnumerable<T>' type
    private readonly IEnumerable<Location> animals;
    private readonly IEnumerable<Location> predators;

    public IEnumerable<Location> Animals { get { return animals; } }
    public IEnumerable<Location> Predators { get { return predators; } }
    
    // Create a new simulation state
    public Simulation(List<Location> animals, List<Location> predators)
    {
      this.animals = animals;
      this.predators = predators;
    }

    // --------------------------------------------------------------------------
    // Utility methods corresponding to those in F#

    // Returns the distance between two specified locations
    static double Distance(Location l1, Location l2) {
      return Math.Sqrt(Math.Pow(l1.X - l2.X, 2.0) + Math.Pow(l1.Y - l2.Y, 2.0));
    }

    // Returns 10 check-points on the path between the specified locations
    static IEnumerable<Location> GetPathPoints(int count, Location pfrom, Location pto) {
      for (double q = 0; q <= 1; q += (1.0 / count))
        yield return pfrom + (pto - pfrom) * q;
    }

    // Returns the specified number of randomly generated locations
    static IEnumerable<Location> RandomLocations(int count) {
      var rnd = SafeRandom.New();
      for (int i = 0; i < count; i++)
        yield return new Location(rnd.NextDouble() * 800, rnd.NextDouble() * 600);
    }

    // --------------------------------------------------------------------------
    // Section 14.3.3 Designing simulation operations


    // Listing 14.26 Implementing the predator behavior (C#)

    // Calculate the number of locations (predators or animals) close to the given position
    int CountCloseLocations(IEnumerable<Location> an, Location pos) {
      return an.Where(a => 50 > Distance(a, pos)).Count();
    }
    // Return the number of locations close to the whole path to 'target'
    int CountCloseLocationsOnPath(Location predPos, IEnumerable<Location> an, Location ptarget) {
      return GetPathPoints(10, predPos, ptarget).Sum(pos => CountCloseLocations(an, pos));
    }
    Location MovePredator(Location predPos) {
      // Choose the best of the generated locations  
      var target = RandomLocations(20).MaxBy(pos =>
        // Prefer path with more animals and less predators
        CountCloseLocationsOnPath(predPos, animals, pos) - CountCloseLocationsOnPath(predPos, predators, pos) * 3);
      // Move the predator by 10 points in that direction
      return predPos + (target - predPos) * (10.0 / Distance(target, predPos));
    }

    // Implementing animal behavior
    Location MoveAnimal(Location animPos) {
      // We can use local lambda functions instead of methods...

      // Get the distance between 'pos' and the nearest predator
      Func<Location, double> nearestPredatorDistanceFrom = (pos) =>
        predators.Min(an => Distance(an, pos));
      
      // Check safety of the path to the 'target'
      Func<Location, double> nearestPredatorDistanceOnPath = ptarget =>
        GetPathPoints(10, animPos, ptarget).Min(nearestPredatorDistanceFrom);

      // Choose the best of the generated locations
      var target = RandomLocations(10).MaxBy(nearestPredatorDistanceOnPath);
      // Move the animal by 20 points in that direction
      return animPos + (target - animPos) * (20.0 / Distance(target, animPos));
    }

    // --------------------------------------------------------------------------
    // Section 14.3.5 Running the simulation in parallel

    public static Simulation CreateInitialState() {
      return new Simulation(RandomLocations(150).ToList(), RandomLocations(15).ToList());
    }

    // Listing 14.28 Runnign the simulation step in parallel
    // Note: the version below includes measuring of the performance
    public Simulation Step() {
      var futureAnims = Task.Factory.StartNew(() =>
        Utils.MeasureTime("moving animals", () =>
          Animals.AsParallel()
            .Select(a => MoveAnimal(a))
            .ToList()));
      var predators =
        Utils.MeasureTime("moving predators", () =>
          Predators.AsParallel()
            .Select(p => MovePredator(p))
            .ToList());
      return new Simulation(futureAnims.Result, predators);
    }
  }  
}