using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using System.Drawing;
using FunctionalCSharp;
using System.Xml.Linq;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 7
// --------------------------------------------------------------------------
// This file contains the object oriented implementation of 
// the document drawing application.
//
// NOTE: This isn't fully commented yet. 
// --------------------------------------------------------------------------

namespace Chapter07_CSharp
{
  #region 'TextContent' representation of a text

  class TextContent
	{
		public TextContent(string text, Font font) {
			Text = text; Font = font;
		}

		public string Text { get; private set; }
		public Font Font { get; private set; }
  }

  #endregion 

  #region 'ScreenElement' representation with drawing

  abstract class ScreenElement
	{
		public abstract void DrawPart(Graphics gr);
	}

	class TextElement : ScreenElement
	{
		public TextElement(TextContent text, RectangleF rect) {
			Text = text; Rect = rect;
		}

		public TextContent Text { get; private set; }
		public RectangleF Rect { get; private set; }

		public override void DrawPart(Graphics gr) {
			gr.DrawString(Text.Text, Text.Font, Brushes.Black, Rect);
		}
	}

	class ImageElement : ScreenElement
	{
		public ImageElement(string path, RectangleF rect) {
			Path = path; Rect = rect;
		}

		public string Path { get; private set; }
		public RectangleF Rect { get; private set; }

		public override void DrawPart(Graphics gr) {
			var bmp = new Bitmap(Path);
      float wspace = Rect.Width / 10.0f, hspace = Rect.Height / 10.0f;
			var rc = new RectangleF(Rect.Left + wspace, Rect.Top + hspace, Rect.Width - 2 * wspace, Rect.Height - 2 * hspace);
			gr.DrawImage(bmp, rc);
		}
	}

	#endregion

	#region 'DocumentPart' representation with visitor class

	// Generic visitor with state
	abstract class DocumentPartVisitor<T>
	{
		public abstract T VisitSplitPart(SplitPart part, T state);
		public abstract T VisitTitledPart(TitledPart part, T state);
		public abstract T VisitTextPart(TextPart part, T state);
		public abstract T VisitImagePart(ImagePart part, T state);
	}

	abstract class DocumentPart 
	{
		public abstract T Accept<T>(DocumentPartVisitor<T> visitor, T state);
		public abstract T Aggregate<T>(Func<T, DocumentPart, T> f, T state);
	}

  class SplitPart : DocumentPart
	{
		public SplitPart(Orientation orientation, FuncList<DocumentPart> parts) {
			Orientation = orientation; Parts = parts;
		}
		public override T Accept<T>(DocumentPartVisitor<T> visitor, T state) {
			return visitor.VisitSplitPart(this, state);
		}
		public override T Aggregate<T>(Func<T, DocumentPart, T> f, T state) {
			T nstate = f(state, this);
			return Parts.Aggregate(nstate, (st, part) => part.Aggregate(f, st));
		}
		public Orientation Orientation { get; private set; }
		public FuncList<DocumentPart> Parts { get; private set; }
	}

  class TitledPart : DocumentPart
	{
		public TitledPart(TextContent text, DocumentPart body) {
			Text = text; Body = body;
		}
		public override T Accept<T>(DocumentPartVisitor<T> visitor, T state) {
			return visitor.VisitTitledPart(this, state);
		}
		public override T Aggregate<T>(Func<T, DocumentPart, T> f, T state) {
			return Body.Aggregate(f, f(state, this));
		}
		public TextContent Text { get; private set; }
		public DocumentPart Body { get; private set; }
	}

  class TextPart : DocumentPart
	{
		public TextPart(TextContent text) {
			Text = text;
		}
		public override T Accept<T>(DocumentPartVisitor<T> visitor, T state) {
			return visitor.VisitTextPart(this, state);
		}
		public override T Aggregate<T>(Func<T, DocumentPart, T> f, T state) {
			return f(state, this);
		}
		public TextContent Text { get; private set; }
	}		

  class ImagePart : DocumentPart
	{
		public ImagePart(string url) {
			Url = url;
		}
		public override T Accept<T>(DocumentPartVisitor<T> visitor, T state) {
			return visitor.VisitImagePart(this, state);
		}
		public override T Aggregate<T>(Func<T, DocumentPart, T> f, T state) {
			return f(state, this);
		}
		public string Url { get; private set; }
	}


	#endregion

  #region Loading of the document from XML files

  static class XmlLoader
  {
    public static string Attribute(this XElement node, string name, string def)
    {
      var attr = node.Attribute(name);
      return attr != null ? attr.Value : def;
    }

    static Font ParseFont(XElement node)
    {
      var styleStr = node.Attribute("style", "");
      var style = styleStr.Contains("bold") ? FontStyle.Bold : FontStyle.Regular;
      style = styleStr.Contains("italic") ? (style | FontStyle.Italic) : style;
      return new Font(node.Attribute("font", "Calibri"), Single.Parse(node.Attribute("size", "12")), style);
    }

    static Orientation ParseOrientation(string orient)
    {
      return (orient == "horizontal") ? Orientation.Horizontal : Orientation.Vertical;
    }

    static DocumentPart LoadPart(XElement node)
    {
      switch (node.Name.LocalName)
      {
        case "titled":
          return new TitledPart(new TextContent(node.Attribute("title", ""), ParseFont(node)),
            LoadPart(node.Elements().First()));
        case "split":
          var nodes = node.Elements().Select((el) => LoadPart(el)).ToFuncList();
          return new SplitPart(ParseOrientation(node.Attribute("orientation", "")), nodes);
        case "text":
          return new TextPart(new TextContent(node.Value, ParseFont(node)));
        case "image":
          return new ImagePart(node.Attribute("filename", ""));
      }
      throw new Exception("Unknown element!");
    }

    public static DocumentPart LoadDocument(string url)
    {
      return LoadPart(XDocument.Load(url).Root);
    }
  }

  #endregion

  // Implementation of the visitor base class

	#region Translation visitor

	class TranslationState
	{
		public RectangleF Rect { get; set; }
		public FuncList<ScreenElement> Result { get; set; }	
	}

	class DocumentToScreen : DocumentPartVisitor<TranslationState>
	{
		public override TranslationState VisitImagePart(ImagePart part, TranslationState ctx)
		{
			ctx.Result = FuncList.Cons(new ImageElement(part.Url, ctx.Rect), FuncList.Empty<ScreenElement>());
			return ctx;
		}
		public override TranslationState VisitTextPart(TextPart part, TranslationState ctx)
		{
			ctx.Result = FuncList.Cons(new TextElement(part.Text, ctx.Rect), FuncList.Empty<ScreenElement>());
			return ctx;
		}
		public override TranslationState VisitSplitPart(SplitPart part, TranslationState ctx)
		{
			if (part.Orientation == Orientation.Vertical)
			{
				var height = ctx.Rect.Height / part.Parts.Count;
				ctx.Result = part.Parts.Select((p, i) =>
				{
					var rc = new RectangleF(ctx.Rect.Left, ctx.Rect.Top + i * height, ctx.Rect.Width, height);
					return p.Accept(this, new TranslationState { Rect = rc }).Result;
				}).Concat();
			}
			else
			{
				var width = ctx.Rect.Width / part.Parts.Count;
				ctx.Result = part.Parts.Select((p, i) =>
				{
					var rc = new RectangleF(ctx.Rect.Left + i * width, ctx.Rect.Top, width, ctx.Rect.Height);
					return p.Accept(this, new TranslationState { Rect = rc }).Result;
				}).Concat();
			}
			return ctx;
		}
		public override TranslationState VisitTitledPart(TitledPart part, TranslationState ctx)
		{
			var rc = new RectangleF(ctx.Rect.Left, ctx.Rect.Top + 35.0f, ctx.Rect.Width, ctx.Rect.Height - 35.0f);
			var bodyElements = part.Body.Accept(this, new TranslationState { Rect = rc }).Result;
			var titleRc = new RectangleF(ctx.Rect.Left, ctx.Rect.Top, ctx.Rect.Width, 35.0f);
			ctx.Result = FuncList.Cons(new TextElement(part.Text, titleRc), bodyElements);
			return ctx;
		}
	}

	#endregion

  // --------------------------------------------------------------------------
  // Section 7.5.2 Adding functions using visitor 
	
  // Listing 7.17 Counting words in the document using Visitor (C#)

	class CountWordsVisitor : DocumentPartVisitor<int> {
		public override int VisitTitledPart(TitledPart part, int state) {
	    // Recursively count words of the body
	  	return part.Text.Text.Split(' ').Length + part.Body.Accept(this, state);
		}
		public override int VisitSplitPart(SplitPart part, int state) {
      // Aggregate the count over all subparts
			return part.Parts.Aggregate(state, (count, p) => 
        // Count words in each part
        p.Accept(this, count));
		}
		public override int VisitTextPart(TextPart part, int state) {
			return part.Text.Text.Split(' ').Length + state;
		}
		public override int VisitImagePart(ImagePart part, int state) {
			return state;
		}
	}

	static class Program
  {
    #region DrawImage function using 'hole in the middle'

    static Bitmap DrawImage(int wid, int hgt, float space, Action<Graphics> f) {
			var bmp = new Bitmap(wid, hgt);
			var gr = Graphics.FromImage(bmp);
			gr.FillRectangle(Brushes.White, new Rectangle(new Point(0,0), new Size(wid,hgt)));
			gr.TranslateTransform(space, space);
		  f(gr);
		  gr.Dispose();
			return bmp;
    }

    #endregion

    [STAThread]
		static void Main()
		{
      // Load document and convert it to flat representation (using visitor)
			var doc = XmlLoader.LoadDocument("..\\..\\document.xml");
			var screenParts = 
        doc.Accept(
          new DocumentToScreen(), 
          new TranslationState { Rect = new RectangleF(0.0f, 0.0f, 520.0f, 630.0f) }
        ).Result;
		  
      // Show the main form with the document
      var main = new Form() { 
          Text = "Document", 
          ClientSize=new Size(570, 680) 
        };
			main.BackgroundImage = DrawImage(570, 680, 20.0f, (gr) => 
        screenParts.Iter((part) => part.DrawPart(gr)));

      // Run the word-counting visitor and display the result
			int count = doc.Accept(new CountWordsVisitor(), 0);
			MessageBox.Show("Count of the words in the document:" + count.ToString());

			Application.Run(main);
		}
	}
}
