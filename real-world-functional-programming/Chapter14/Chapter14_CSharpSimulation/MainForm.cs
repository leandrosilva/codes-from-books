using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Threading;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 14
// --------------------------------------------------------------------------

namespace Simulation_CSharp
{
  public partial class MainForm : Form
  {
    // --------------------------------------------------------------------------
    // Section 14.3.3 Designing simulation operations

    // Listing 14.23 Running simulation on a thread
    private void MainForm_Load(object sender, EventArgs e) {
      th = new Thread(() => {
        // Mutable variable that holds the current state
        bool running = true;
        Simulation state = Simulation.CreateInitialState();
        while (running) {
          // Calculate the new state
          state = state.Step();
          running = (bool)this.Invoke(new Func<bool>(() => 
            // Redraw the form
            DrawState(state)));
        }
      });
      // Start the simulation loop thread
      th.Start();
    }

    // --------------------------------------------------------------------------
    // Implementing GUI (This code isn't discussed in the book)

    public bool DrawState(Simulation state)
    {
      if (!this.Visible) return false;

      var bmp = new Bitmap(ClientSize.Width, ClientSize.Height);
      using(var gr = Graphics.FromImage(bmp))
      {
        // Fill the background
				gr.FillRectangle(Brushes.Khaki, this.ClientRectangle);

        // Draw all the animals and predatorsd
        foreach(var pred in state.Predators)
          gr.FillEllipse(Brushes.OrangeRed, new Rectangle((int)pred.X, (int)pred.Y, 20, 20));
        foreach(var anim in state.Animals)
          gr.FillEllipse(Brushes.Green, new Rectangle((int)anim.X, (int)anim.Y, 10, 10));
      }
      // Draw the 'cached' bitmap to the form
      using (var gr = this.CreateGraphics())
        gr.DrawImage(bmp, new Point(0, 0));

      return true;
    }

    // Keep the thread as a fied, so that we can stop it
		Thread th;

    public MainForm() {
      InitializeComponent();
      ClientSize = new Size(800, 600);
    }

		private void MainForm_FormClosing(object sender, FormClosingEventArgs e) {
			Visible = false;
      // Wait for the thread to finish
			th.Join();
		}
  }
}
