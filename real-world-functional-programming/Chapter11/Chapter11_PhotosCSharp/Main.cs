using System;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;
using System.IO;
using System.Drawing.Drawing2D;
using FunctionalCSharp;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 11
// --------------------------------------------------------------------------
// The C# version of this sample application isn't included in the book
// --------------------------------------------------------------------------

namespace Chapter11_PhotoApp_CSharp
{
	public partial class MainForm : Form
	{
		public MainForm()
		{
			InitializeComponent();
		}

    // --------------------------------------------------------------------------
    // Browsing photos in C#

		private void Form1_Load(object sender, EventArgs e)
		{
      // Specify directory with your photos here!
      var dir = @"C:\Users\Public\Pictures\Sample Pictures";

      // Use 'Select' to return information for each photo
      var files = Directory.GetFiles(dir, "*.jpg").Select(file =>  
        // Anonymous type
        new { 
          // Return name and lazy value with the resized image
          Name = Path.GetFileName(file),
          Preview = CreateLazyResized(file) });

      var filesArray = files.ToArray();
			list.SelectedIndexChanged += (sender2, e2) => {
         // Force evaluation and show the bitmap
				pict.Image = filesArray[list.SelectedIndex].Preview.Value;
			};
      
      // Setup the data binding
			list.DataSource = filesArray;
      list.DisplayMember = "Name";
		}

    // Creataes a lazy value that eventually resizes the image
    private static Lazy<Bitmap> CreateLazyResized(string file)
    {
      return Lazy.Create(() => {
        // Draw the resized bitmap..
        var bmp = Bitmap.FromFile(file);
        var resized = new Bitmap(400, 300);
        using (var gr = Graphics.FromImage(resized))
        {
          var dst = new Rectangle(0, 0, 400, 300);
          var src = new Rectangle(0, 0, bmp.Width, bmp.Height);
          gr.InterpolationMode = InterpolationMode.High;
          gr.DrawImage(bmp, dst, src, GraphicsUnit.Pixel);
        }
        return resized;
      });
    }
	}
}
