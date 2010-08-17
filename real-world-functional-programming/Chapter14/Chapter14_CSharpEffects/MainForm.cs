using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using FunctionalCSharp;
using System.Diagnostics;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 14
// --------------------------------------------------------------------------

namespace GraphicalFilters_CSharp
{
  public partial class MainForm : Form
	{
    // --------------------------------------------------------------------------
    // Section 14.2.3 Designing the main application

    // Listing 14.12 Representation of graphical effect
		class EffectInfo
		{
      // Function representing the effect
			public Func<SimpleColor[,], SimpleColor[,]>  Effect { get; set; }
			// The name of the effect
      public string Name { get; set; }
			public override string ToString() {
				return Name;
			}
		}


    // --------------------------------------------------------------------------
    // Section 14.2.4 Creating and running effects

		public MainForm()
		{
			InitializeComponent();

      // Initialize the collection of effects (created from filters)
			var effects =
				new List<EffectInfo> {
					new EffectInfo { Name = "Grayscale (sequential)", Effect = Filters.MakeEffect(Filters.Grayscale) },
					new EffectInfo { Name = "Grayscale (parallel)", Effect = Filters.MakeParallelEffect(Filters.Grayscale) },
					new EffectInfo { Name = "Lighten (sequential)", Effect = Filters.MakeEffect(Filters.Lighten) },
					new EffectInfo { Name = "Lighten (parallel)", Effect = Filters.MakeParallelEffect(Filters.Lighten) },
          
          // Bonus: Add one effect implemented directly as an effect
          new EffectInfo { Name = "Blur (sequential)", Effect = Effects.SequentialBlur },
          new EffectInfo { Name = "Blur (parallel)", Effect = Effects.ParallelBlur }
				};

      // Setup the databinding to display the list of effects
			listFilters.ComboBox.DataSource = effects;
			listFilters.ComboBox.DisplayMember = "Name";
			listFilters.SelectedIndex = 0;
		}

    // Listing 14.15 Measuring the time in F# and C#
    public static Tuple<T, long> MeasureTime<T>(Func<T> f) {
      var stop = Stopwatch.StartNew();
      var res = f();
      return Tuple.Create(res, stop.ElapsedMilliseconds);
    }

    // Listing 14.16 Applying selected effect to a bitmap
    private void btnRun_Click(object sender, EventArgs e) {
      if (loadedBitmap == null) return;

      // Get the selected filter
      var filter = ((EffectInfo)listFilters.SelectedItem).Effect;
      var arr = loadedBitmap.ToArray2D();

      // Run filter and measure performance
      var res = MeasureTime(() => filter(arr));

      // Display result and time taken
      pictProcessed.Image = res.Item1.ToBitmap();
      lblTime.Text = string.Format("Time: {0} ms", res.Item2);
    }

    // --------------------------------------------------------------------------
    // The rest of the code implements simple GUI interactions...
  
		Bitmap loadedBitmap;

		private void btnOpen_Click(object sender, EventArgs e) {
			if (openImageDlg.ShowDialog() == DialogResult.OK)
				pictOriginal.Image = loadedBitmap = new Bitmap(openImageDlg.FileName);
		}

		private void btnUseProcessed_Click(object sender, EventArgs e) {
			if (pictProcessed.Image != null) {
				pictOriginal.Image = pictProcessed.Image;
				loadedBitmap = (Bitmap)pictProcessed.Image;
			}
		}
	}
}
