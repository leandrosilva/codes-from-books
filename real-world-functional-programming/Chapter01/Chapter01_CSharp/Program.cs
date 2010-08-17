using System;
using System.Linq;
using System.Drawing;
using System.Windows.Forms;
using System.Collections.Generic;
using Animations;
#if NET_4
using System.Threading.Tasks;
#endif

namespace Chapter01_CSharp
{
	// --------------------------------------------------------------------------
	// Functional Programming in .NET - Chapter 1
	// --------------------------------------------------------------------------
	// NOTE: The source demonstrates how we'll write solutions to several
	// problems in the later chapters, so it is just the code used in the chapter
	// with necessary class declarations etc. to make it compile. But the code
	// doesn't do anything useful yet.
	// --------------------------------------------------------------------------

	class Program
	{
		#region Sample products

		class Product
		{
			public int ProductID { get; set; }
			public int CategoryID { get; set; }
			public string ProductName { get; set; }
			public decimal UnitPrice { get; set; }
		}

		static IEnumerable<Product> Products
		{
			get
			{
				return new List<Product> 
				{
					new Product { ProductID=1, CategoryID=1, ProductName="Chai", UnitPrice=18.0000M },
					new Product { ProductID=2, CategoryID=1, ProductName="Chang", UnitPrice=19.0000M },
					new Product { ProductID=3, CategoryID=2, ProductName="Aniseed Syrup", UnitPrice=10.0000M },
					new Product { ProductID=4, CategoryID=2, ProductName="Chef Anton's Cajun Seasoning", UnitPrice=22.0000M },
					new Product { ProductID=5, CategoryID=2, ProductName="Chef Anton's Gumbo Mix", UnitPrice=21.3500M },
					new Product { ProductID=6, CategoryID=2, ProductName="Grandma's Boysenberry Spread", UnitPrice=25.0000M },
					new Product { ProductID=7, CategoryID=7, ProductName="Uncle Bob's Organic Dried Pears", UnitPrice=30.0000M },
					new Product { ProductID=8, CategoryID=2, ProductName="Northwoods Cranberry Sauce", UnitPrice=40.0000M },
					new Product { ProductID=9, CategoryID=6, ProductName="Mishi Kobe Niku", UnitPrice=97.0000M },
					new Product { ProductID=10, CategoryID=8, ProductName="Ikura", UnitPrice=31.0000M },
					new Product { ProductID=11, CategoryID=4, ProductName="Queso Cabrales", UnitPrice=21.0000M },
					new Product { ProductID=12, CategoryID=4, ProductName="Queso Manchego La Pastora", UnitPrice=38.0000M },
					new Product { ProductID=13, CategoryID=8, ProductName="Konbu", UnitPrice=6.0000M },
					new Product { ProductID=14, CategoryID=7, ProductName="Tofu", UnitPrice=23.2500M },
					new Product { ProductID=15, CategoryID=2, ProductName="Genen Shouyu", UnitPrice=15.5000M },
					new Product { ProductID=16, CategoryID=3, ProductName="Pavlova", UnitPrice=17.4500M },
					new Product { ProductID=17, CategoryID=6, ProductName="Alice Mutton", UnitPrice=39.0000M },
					new Product { ProductID=18, CategoryID=8, ProductName="Carnarvon Tigers", UnitPrice=62.5000M },
					new Product { ProductID=19, CategoryID=3, ProductName="Teatime Chocolate Biscuits", UnitPrice=9.2000M },
					new Product { ProductID=20, CategoryID=3, ProductName="Sir Rodney's Marmalade", UnitPrice=81.0000M },
					new Product { ProductID=21, CategoryID=3, ProductName="Sir Rodney's Scones", UnitPrice=10.0000M },
					new Product { ProductID=22, CategoryID=5, ProductName="Gustaf's Knäckebröd", UnitPrice=21.0000M },
					new Product { ProductID=23, CategoryID=5, ProductName="Tunnbröd", UnitPrice=9.0000M },
					new Product { ProductID=24, CategoryID=1, ProductName="Guaraná Fantástica", UnitPrice=4.5000M },
					new Product { ProductID=25, CategoryID=3, ProductName="NuNuCa Nuß-Nougat-Creme", UnitPrice=14.0000M },
					new Product { ProductID=26, CategoryID=3, ProductName="Gumbär Gummibärchen", UnitPrice=31.2300M },
					new Product { ProductID=27, CategoryID=3, ProductName="Schoggi Schokolade", UnitPrice=43.9000M },
					new Product { ProductID=28, CategoryID=7, ProductName="Rössle Sauerkraut", UnitPrice=45.6000M },
					new Product { ProductID=29, CategoryID=6, ProductName="Thüringer Rostbratwurst", UnitPrice=123.7900M },
					new Product { ProductID=30, CategoryID=8, ProductName="Nord-Ost Matjeshering", UnitPrice=25.8900M },
					new Product { ProductID=31, CategoryID=4, ProductName="Gorgonzola Telino", UnitPrice=12.5000M },
					new Product { ProductID=32, CategoryID=4, ProductName="Mascarpone Fabioli", UnitPrice=32.0000M },
					new Product { ProductID=33, CategoryID=4, ProductName="Geitost", UnitPrice=2.5000M },
					new Product { ProductID=34, CategoryID=1, ProductName="Sasquatch Ale", UnitPrice=14.0000M },
					new Product { ProductID=35, CategoryID=1, ProductName="Steeleye Stout", UnitPrice=18.0000M },
					new Product { ProductID=36, CategoryID=8, ProductName="Inlagd Sill", UnitPrice=19.0000M },
					new Product { ProductID=37, CategoryID=8, ProductName="Gravad lax", UnitPrice=26.0000M },
					new Product { ProductID=38, CategoryID=1, ProductName="Côte de Blaye", UnitPrice=263.5000M },
					new Product { ProductID=39, CategoryID=1, ProductName="Chartreuse verte", UnitPrice=18.0000M },
					new Product { ProductID=40, CategoryID=8, ProductName="Boston Crab Meat", UnitPrice=18.4000M },
					new Product { ProductID=41, CategoryID=8, ProductName="Jack's New England Clam Chowder", UnitPrice=9.6500M },
					new Product { ProductID=42, CategoryID=5, ProductName="Singaporean Hokkien Fried Mee", UnitPrice=14.0000M },
					new Product { ProductID=43, CategoryID=1, ProductName="Ipoh Coffee", UnitPrice=46.0000M },
					new Product { ProductID=44, CategoryID=2, ProductName="Gula Malacca", UnitPrice=19.4500M },
					new Product { ProductID=45, CategoryID=8, ProductName="Rogede sild", UnitPrice=9.5000M },
					new Product { ProductID=46, CategoryID=8, ProductName="Spegesild", UnitPrice=12.0000M },
					new Product { ProductID=47, CategoryID=3, ProductName="Zaanse koeken", UnitPrice=9.5000M },
					new Product { ProductID=48, CategoryID=3, ProductName="Chocolade", UnitPrice=12.7500M },
					new Product { ProductID=49, CategoryID=3, ProductName="Maxilaku", UnitPrice=20.0000M },
					new Product { ProductID=50, CategoryID=3, ProductName="Valkoinen suklaa", UnitPrice=16.2500M },
					new Product { ProductID=51, CategoryID=7, ProductName="Manjimup Dried Apples", UnitPrice=53.0000M },
					new Product { ProductID=52, CategoryID=5, ProductName="Filo Mix", UnitPrice=7.0000M },
					new Product { ProductID=53, CategoryID=6, ProductName="Perth Pasties", UnitPrice=32.8000M },
					new Product { ProductID=54, CategoryID=6, ProductName="Tourticre", UnitPrice=7.4500M },
					new Product { ProductID=55, CategoryID=6, ProductName="Pâté chinois", UnitPrice=24.0000M },
					new Product { ProductID=56, CategoryID=5, ProductName="Gnocchi di nonna Alice", UnitPrice=38.0000M },
					new Product { ProductID=57, CategoryID=5, ProductName="Ravioli Angelo", UnitPrice=19.5000M },
					new Product { ProductID=58, CategoryID=8, ProductName="Escargots de Bourgogne", UnitPrice=13.2500M },
					new Product { ProductID=59, CategoryID=4, ProductName="Raclette Courdavault", UnitPrice=55.0000M },
					new Product { ProductID=60, CategoryID=4, ProductName="Camembert Pierrot", UnitPrice=34.0000M },
					new Product { ProductID=61, CategoryID=2, ProductName="Sirop d'érable", UnitPrice=28.5000M },
					new Product { ProductID=62, CategoryID=3, ProductName="Tarte au sucre", UnitPrice=49.3000M },
					new Product { ProductID=63, CategoryID=2, ProductName="Vegie-spread", UnitPrice=43.9000M },
					new Product { ProductID=64, CategoryID=5, ProductName="Wimmers gute Semmelknödel", UnitPrice=33.2500M },
					new Product { ProductID=65, CategoryID=2, ProductName="Louisiana Fiery Hot Pepper Sauce", UnitPrice=21.0500M },
					new Product { ProductID=66, CategoryID=2, ProductName="Louisiana Hot Spiced Okra", UnitPrice=17.0000M },
					new Product { ProductID=67, CategoryID=1, ProductName="Laughing Lumberjack Lager", UnitPrice=14.0000M },
					new Product { ProductID=68, CategoryID=3, ProductName="Scottish Longbreads", UnitPrice=12.5000M },
					new Product { ProductID=69, CategoryID=4, ProductName="Gudbrandsdalsost", UnitPrice=36.0000M },
					new Product { ProductID=70, CategoryID=1, ProductName="Outback Lager", UnitPrice=15.0000M },
					new Product { ProductID=71, CategoryID=4, ProductName="Flotemysost", UnitPrice=21.5000M },
					new Product { ProductID=72, CategoryID=4, ProductName="Mozzarella di Giovanni", UnitPrice=34.8000M },
					new Product { ProductID=73, CategoryID=8, ProductName="Röd Kaviar", UnitPrice=15.0000M },
					new Product { ProductID=74, CategoryID=7, ProductName="Longlife Tofu", UnitPrice=10.0000M },
					new Product { ProductID=75, CategoryID=1, ProductName="Rhönbräu Klosterbier", UnitPrice=7.7500M },
					new Product { ProductID=76, CategoryID=1, ProductName="Lakkalikööri", UnitPrice=18.0000M },
					new Product { ProductID=77, CategoryID=2, ProductName="Original Frankfurter grüne Soße", UnitPrice=13.0000M },
				};
			}
		}
		
		#endregion

    // Program entry-point
		static void Main(string[] args)
		{
      // GetExpensiveProductsImperative();
      // GetExpensiveProductsFunctional();
      // Application.Run(new MyForm());
      FunctionalAnim();
		}

    // ------------------------------------------------------------------------
    // Section 1.4.1 Expressing intentions using declarative style

    
    // Listing 1.3 Imperative data processing
    public static IEnumerable<string> GetExpensiveProductsImperative()
    {
      // Create resulting list
      List<string> res = new List<string>();
      // Iterate over products
      foreach (Product p in Products) {
        if (p.UnitPrice > 75.0M) {
          // Add information to list of results 
          res.Add(string.Format("{0} (${1})", p.ProductName, p.UnitPrice));
        }
      }
      return res;
    }

    // Listing 1.4 Declarative data processing
    public static IEnumerable<string> GetExpensiveProductsFunctional()
		{
			var prods =
				from p in Products
        // Filter products using predicate
				where p.UnitPrice > 75.0M 
        // Return information about product
				select string.Format("{0} (${1})", p.ProductName, p.UnitPrice);
			return prods;
		}

    // ------------------------------------------------------------------------
    // Declarative user interfaces in XAML

    // Listing 1.5 Creating user interface using imperative and declarative style
    class MyForm : Form
    {
      protected override void OnPaint(PaintEventArgs e)
      {
        e.Graphics.FillRectangle(Brushes.Black, ClientRectangle);
        e.Graphics.FillEllipse(Brushes.LightGreen, 0, 0, 75, 75);
      }
    }

    // ------------------------------------------------------------------------
    // Declarative functional animations

    // Listing 1.6 Creating functional animation
    static void FunctionalAnim()
    {
      // Create green and blue ellipse
      var greenCircle = Anims.Cirle(
        Time.Forever(Brushes.OliveDrab), 100.0f.Forever());
      var blueCircle = Anims.Cirle(
        Time.Forever(Brushes.SteelBlue), 100.0f.Forever());

      // Value animated from -100 to +100
      var animatedPos = Time.Wiggle * 100.0f.Forever();

      // Animate X or Y coordinates of ellipses
      var greenMove = greenCircle.Translate(animatedPos, 0.0f.Forever());
      var blueMove = blueCircle.Translate(0.0f.Forever(), animatedPos);

      // Compose animation from both ellipses
      var animation = Anims.Compose(greenMove, blueMove);

      // Run the animation
      Application.Run(new AnimationForm() { Animation = animation });
    }

		// ------------------------------------------------------------------------
    // Section 1.4.2 Understanding code using immutability

    // Listing 1.7 Immutable representation of a game character
		public class GameCharacter
		{
      // All fields are declared as readonly
			readonly int health;
			readonly Point location;

			public GameCharacter(int health, Point location) {
        // Initialize immutable fields only once
				this.health = health;
				this.location = location;
			}
			
      public GameCharacter HitByShooting(Point target) {
				int newHealth = CalculateHealth(target);
        // Return a game character with updated health
				return new GameCharacter(newHealth, this.location);
			}

			public bool IsAlive {
				get { return health > 0; }
			}

      // A couple of methods that are just stubs and don't do anything useful:
      #region Mock methods

      int CalculateHealth(Point target) {
				return 100;
			}
      public GameCharacter PerformStep() {
        return new GameCharacter(health, location);
      }
      public bool IsCloseTo(GameCharacter other) {
        return false;
      }

      #endregion
    }

    // Reading Functional Programs

    static void ReadingFuncProgs()
    {
      var monster = new GameCharacter(0, Point.Empty);
      var player = new GameCharacter(0, Point.Empty);
      var gunShot = new Point();

      // Listing 1.8 Code snippets from a functional game

      // Move the monster & test if the player is in danger
      var movedMonster = monster.PerformStep();       // Move the monster
      var inDanger = player.IsCloseTo(movedMonster);  // Test distance from the moved monster

      // Did gunshot hit a monster or the player?
      var hitMonster = monster.HitByShooting(gunShot);  // Create new monster
      var hitPlayer = player.HitByShooting(gunShot);    // .. and player

    }

    // ------------------------------------------------------------------------
    // 1.4.3 Writing efficient parallel programs

    // Parallelizing Immutable Programs

    static void ParallelImmutable() 
    {
      var monster = new GameCharacter(0, Point.Empty);
      var player = new GameCharacter(0, Point.Empty);
      var gunShot = Point.Empty;

      // Task Parallel Library is available only on .NET 4.0
      #if NET_4
      var hitMonster = Task.Factory.StartNew(() =>
        monster.HitByShooting(gunShot));
      var hitPlayer = Task.Factory.StartNew(() =>
        player.HitByShooting(gunShot));
      #endif
    }

    // Declarative parallelism using LINQ

    // Listing 1.9 Parallelizing data processing code using PLINQ (C#)
    static void DeclarativeParallelism() 
    {
      var monsters = new List<GameCharacter>();

      // Single-threaded version
      var updated1 = 
         from m in monsters
         let nm = m.PerformStep()
         where nm.IsAlive select nm;

      // Parallel version
      // PLINQ is available only on .NET 4.0
      #if NET_4
      var updated2 = 
         from m in monsters.AsParallel()
         let nm = m.PerformStep()
         where nm.IsAlive select nm;
      #endif
    }
	}
}

