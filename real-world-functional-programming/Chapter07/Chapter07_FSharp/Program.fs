open System.Windows.Forms

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 7
// --------------------------------------------------------------------------
// This file contains the whole implementation of 
// the document drawing application.
// --------------------------------------------------------------------------

// The following types & functions are commented in 'Script.fsx' 

type Rect =   
  { Left : float32
    Top : float32
    Width : float32
    Height : float32 }

open System.Drawing

let deflate(original, wspace, hspace) = 
  { Top = original.Top + wspace
    Left = original.Left + hspace
    Width = original.Width - (2.0f * wspace)
    Height = original.Height - (2.0f * hspace) }
  
let toRectangleF(original) = 
  RectangleF(original.Left, original.Top, 
             original.Width, original.Height)

// ----------------------------------------------------------------------------
// Section 7.2 Flat document representation

// Listing 7.4 Flat document representation (F#)
open System.Drawing

// Represents text with font
type TextContent = 
  { Text : string
    Font : Font }

// Represents element of the document
type ScreenElement = 
  // Text content with bounding box
  | TextElement of TextContent * Rect
  // Image file name with bounding box
  | ImageElement of string * Rect


// ----------------------------------------------------------------------------
// Section 7.2.1 Drawing documents
 
// Listing 7.5 Drawing document using flat representation 

let drawElements elements (gr:Graphics) = 
  // Imperative iteration over all elements
  for el in elements do
    match el with
    | TextElement(text, boundingBox) -> 
        // Convert 'Rect' to .NET RectangleF
        gr.DrawString(text.Text, text.Font, Brushes.Black, toRectangleF(boundingBox))
    | ImageElement(img, rc) ->
        // Load image from the specified file
        let bmp = new Bitmap(img)
        // Add border 10% of the image size
        let wspace, hspace = 
          rc.Width / 10.0f, rc.Height / 10.0f
        let rc = toRectangleF(deflate(rc, wspace, hspace))
        gr.DrawImage(bmp, rc)

// Listing 7.6 Function for drawing images
let drawImage (width:int, height:int) space coreDrawingFunc =
  let bmp = new Bitmap(width, height)
  let gr = Graphics.FromImage(bmp)
  // Fill the background with white color 
  gr.Clear(Color.White)
  // All drawing on graphics will be shifted
  gr.TranslateTransform(space, space)
  // Call the core part of drawing
  coreDrawingFunc(gr)
  // Finalization
  gr.Dispose()
  bmp
  
// ----------------------------------------------------------------------------
// Creating sample document using the representation above
// Note: you can use F# interactive to run the code below

#if INTERACTIVE
open System.Windows.Forms

let fntText = new Font("Calibri", 12.0f)
let fntHead = new Font("Calibri", 15.0f)

let parts = 
  [ TextElement
     ({ Text = "Functional Programming for the Real World"; Font = fntHead }, 
      { Left = 5.0f; Top = 0.0f; Width = 410.0f; Height = 30.0f });
    ImageElement
     ("C:\\Tomas\\Writing\\Functional\\Petricek.jpg", 
      { Left = 120.0f; Top = 30.0f; Width = 150.0f; Height = 200.0f });
    TextElement
     ({ Text = "In this book, we'll introduce you to the essential "+
         "concepts of functional programming, but thanks to the .NET "+ 
         "Framework, we won't be limited to theoretical examples. We'll "+ 
         "use many of the rich .NET libraries to show how functional "+
         "programming can be used in the real-world."; Font = fntText }, 
     { Left = 10.0f; Top = 230.0f; Width = 400.0f; Height = 400.0f }) ]

let main = new Form(Text = "Document", ClientSize=Size(450, 400))
main.BackgroundImage <- drawImage (450, 400) 20.0f (drawElements parts)
main.Show()

#endif

// ----------------------------------------------------------------------------
// Section 7.3 Structured document representation

// Listing 7.8 Hierarchical document representation

// Represents orientation of the 'SplitPart' 
type Orientation = 
  | Vertical
  | Horizontal 

// Recursive type representing the document
type DocumentPart = 
  // Columns or rows containing parts
  | SplitPart of Orientation * DocumentPart list
  // Other part with a title
  | TitledPart of TextContent * DocumentPart
  // Simple content parts
  | TextPart of TextContent
  | ImagePart of string

// ----------------------------------------------------------------------------
// Section 7.3.1 Converting representations

// Listing 7.9 Translation between document representations

let rec documentToScreen(doc, boundingBox) = 
  match doc with
  | SplitPart(Horizontal, parts) ->
      // Calculate the size of individual parts
      let width = boundingBox.Width / (float32 parts.Length)
      parts 
        // Recursively translate columns of part
        |> List.mapi (fun i part -> 
          let boundingBox = { boundingBox with Left = boundingBox.Left + float32(i) * width; Width = width }
          documentToScreen(part, boundingBox) )
        // Concatenate lists of screen elements
        |> List.concat
        
  | SplitPart(Vertical, parts) ->
      let height = boundingBox.Height / (float32 parts.Length)
      parts 
        |> List.mapi (fun i part -> 
          // Calculate bounding box of the row
          let boundingBox = { boundingBox with Top = boundingBox.Top + (float32 i) * height; Height = height }
          // Recursive call
          documentToScreen(part, boundingBox) )
        |> List.concat
        
  | TitledPart(tx, doc) ->
      let titleRc = { boundingBox with Height = 35.0f }
      let restRc = { boundingBox with Top = boundingBox.Top + 35.0f; Height = boundingBox.Height - 35.0f }
      // Translate the body and append the title
      TextElement(tx, titleRc)::(documentToScreen(doc, restRc))
      
  | TextPart(tx) -> [ TextElement(tx, boundingBox) ]
  | ImagePart(im) -> [ ImageElement(im, boundingBox) ]

// ----------------------------------------------------------------------------
// Section 7.3.3 XML document representation

// Listing 7.11 Parsing font and orientation using LINQ to XML (F#)

open System.Xml.Linq

// Reads attribute or returns the specified default value
let attr(node:XElement, name, defaultValue) =
  let attr = node.Attribute(XName.Get(name))
  if (attr <> null) then attr.Value else defaultValue

// Parses value of the 'orientation' attribute 
let parseOrientation(node) = 
  match attr(node, "orientation", "") with
  | "horizontal" -> Horizontal
  | "vertical" -> Vertical  
  | _ -> failwith "Invalid orientation!"

// Parse a font node with specified style 
let parseFont(node) =
  let value = attr(node, "style", "")
  let style = 
      // Tests whether the attribute contains specified strings
      match value.Contains("bold"), value.Contains("italic") with
      | true,  false -> FontStyle.Bold
      | false, true  -> FontStyle.Italic
      | true,  true  -> 
          // Combine two options of .NET enumeration
          FontStyle.Bold ||| FontStyle.Italic
      | false, false -> FontStyle.Regular
  let name = attr(node, "font", "Calibri")
  new Font(name, float32(attr(node, "size", "12")), style)


// Listing 7.12 Loading document parts from XML

let rec loadPart(node:XElement) =
  // Select branch using element name
  match node.Name.LocalName with
  | "titled" -> 
      let tx = { Text = attr(node, "title", ""); Font = parseFont node}
      // Recursively load the first child element
      let body = loadPart(Seq.head(node.Elements()))
      TitledPart(tx, body)
  | "split"  -> 
      let orient = parseOrientation(node)
      // Recursively load all children
      let nodes = node.Elements() |> List.ofSeq |> List.map loadPart
      SplitPart(orient, nodes)
  | "text"   -> TextPart({Text = node.Value; Font = parseFont node})
  | "image"  -> ImagePart(attr(node, "filename", ""))
  | name -> 
      // Throw an exception
      failwith("Unknown node: " + name)

// Note: Most of the code from listing 7.13 is moved below, so that it 
// can display documents generated by the next couple of functions..  
let doc = loadPart(XDocument.Load(@"..\..\document2.xml").Root)

// ----------------------------------------------------------------------------
// Section 7.4.1 Updating using map operation

// Listing 7.14 Map operation for documents

let rec mapDocument f doc = 
  // Tests the result and the original part
  let processed = 
    match doc with 
    | TitledPart(tx, cont) ->
        // Recursively process the body
        TitledPart(tx, mapDocument f cont)
    | SplitPart(orientation, parts) ->
        // Process all columns or rows 
        SplitPart(orientation, parts |> List.map (mapDocument f))
    | _ -> doc
  f(processed)    

// Listing 7.15 Shrinking split part containing text

// Utility testing whether part is a 'TextPart'
let isText(part) = 
  match part with | TextPart(_) -> true | _ -> false

let shrinkDocument part =
  match part with
  // Split part containing only text parts
  | SplitPart(_, parts) when List.forall isText parts -> 
      // Aggregate all parts using fold
      let res = 
          List.fold (fun (st) (TextPart(tx)) -> 
            // Concatenate text and return font
            { Text = st.Text + " " + tx.Text
              Font = tx.Font } ) 
            { Text = ""; Font = null } parts // Start with empty string and 'null' font
      TextPart(res)
  | _ -> 
      // Ignore other cases
      part
      
let shrinkedDoc = doc |> mapDocument shrinkDocument

// ----------------------------------------------------------------------------
// Section 7.4.2 Calculating using aggregate operation

// Listing 7.16 Aggregation of document parts

let rec aggregateDocument f state doc = 
  // Calculate new state for the current part
  let state = f state doc
  match doc with 
  | TitledPart(_, part) ->
      // Recursively process the body
      aggregateDocument f state part
  | SplitPart(_, parts) ->
      // Aggregate state over all subparts
      List.fold (aggregateDocument f) state parts
  | _ -> state
  
  
// Usinng the function above to count characters of a document
let totalChars =
  aggregateDocument (fun count part -> 
    match part with
    | TextPart(tx) | TitledPart(tx, _) -> count + tx.Text.Split([| ' ' |]).Length
    | _ -> count) 0 doc

MessageBox.Show("The number of characters of the document is: " + totalChars.ToString()) |> ignore

// ----------------------------------------------------------------------------

// Listing 7.13 Putting parts of the application together (F#)
[<System.STAThread>]
do
  // Minor difference from the book:
  // here we wrap the code to show the form inside a function
  let showDoc(doc) =
    let rc = { Left = 0.0f; Top = 0.0f; 
               Width = 520.0f; Height = 630.0f }
    let parts = documentToScreen(doc, rc)
    let img = drawImage (570, 680) 25.0f (drawElements parts)
    let frm = new Form(Text = "Document", BackgroundImage = img,
                       ClientSize = Size(570, 680) )
    frm.Show()
  
  showDoc(doc)
  showDoc(shrinkedDoc)
  Application.Run()
