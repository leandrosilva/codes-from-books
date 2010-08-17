using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 7
// --------------------------------------------------------------------------
// This file contains code samples that are not related to the primary
// example of the chapter (document drawing application).
// --------------------------------------------------------------------------

namespace Chapter07_CSharp_Introduction
{
  // --------------------------------------------------------------------------
  // Section 7.1.2 Functional data structures in C#

  // Listing 7.3 Immutable 'Rect' type (C#)
  class Rect
  {
    // Readonly properties of the type
    private readonly float left, top, width, height;

    public float Left { get { return left; } }
    public float Top { get { return top; } }
    public float Width { get { return width; } }
    public float Height { get { return height; } }

    // Construct the value
    public Rect(float left, float top, float width, float height) {
      this.left = left; this.top = top; this.width = width; this.height = height;
    }

    // Returns 'Rect' with modified 'Left' property
    public Rect WithLeft(float left) {
      // Create a copy of the object
      return new Rect(left, this.Top, this.Width, this.Height);
    }

    // Other 'With' methods are similar...
		public Rect WithTop(float top) {
			return new Rect(this.Left, top, this.Width, this.Height);
		}

		public Rect WithWidth(float width) {
			return new Rect(this.Left, this.Top, width, this.Height);
		}

		public Rect WithHeight(float height) {
			return new Rect(this.Left, this.Top, this.Width, height);
		}
	}

  // Working with immutable types

	class Demo
	{
		static void IntroMain()
		{
			Rect rc = new Rect(0.0f, 0.0f, 100.0f, 100.0f);
      var moved = rc.WithLeft(10.0f).WithTop(10.0f);
		}
	}
}
