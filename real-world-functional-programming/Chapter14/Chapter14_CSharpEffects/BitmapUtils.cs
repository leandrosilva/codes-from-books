using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Imaging;

namespace GraphicalFilters_CSharp
{
  static class BitmapUtils {
    // Convert bitmap to an array
    public static SimpleColor[,] ToArray2D(this Bitmap bmp) {
			var rect = new Rectangle(0, 0, bmp.Width, bmp.Height);
      var bmpData = bmp.LockBits(rect, ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb);
      var ptr0 = bmpData.Scan0;
      var stride = bmpData.Stride;
      var res = new SimpleColor[bmp.Width, bmp.Height];
			int width = bmp.Width;
			int height = bmp.Height;
			for (int y = 0; y < width; y++) {
				for(int x = 0; x < height; x++) {
					var offset = y*4 + stride*x;
					var clr = Color.FromArgb(System.Runtime.InteropServices.Marshal.ReadInt32(ptr0,offset));
					res[y,x] = new SimpleColor(clr.R, clr.G, clr.B);
				}
			}
			bmp.UnlockBits(bmpData);
      return res;
		}

    // Convert array to .NET Bitmap type
    public static Bitmap ToBitmap(this SimpleColor[,] arr) {
			var bmp = new Bitmap(arr.GetLength(0), arr.GetLength(1));
      var rect = new Rectangle(0, 0, bmp.Width, bmp.Height);
      var bmpData = bmp.LockBits(rect, ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb);
      var ptr0 = bmpData.Scan0;
      var stride = bmpData.Stride;
			int width = bmp.Width;
			int height = bmp.Height;
      for(int y = 0; y < width; y++) {
				for(int x = 0; x < height; x++) {
          var offset = y*4 + stride*x;
					var clr = arr[y, x];
          var clr2 = Color.FromArgb(clr.R, clr.G, clr.B).ToArgb();
          System.Runtime.InteropServices.Marshal.WriteInt32(ptr0, offset, clr2);
				}
			}
			bmp.UnlockBits(bmpData);
      return bmp;
		}
	}
}
