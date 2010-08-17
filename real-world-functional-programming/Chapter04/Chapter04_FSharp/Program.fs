// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 4
// --------------------------------------------------------------------------

open System
open System.IO

// ----------------------------------------------------------------------------
// Code shared by both versions

// Section 4.2.1: Loading and processing data

// Listing 4.2: Parsing a row from the file
// Convert line from the CSV file to (label, number) tuple
let convertDataRow(csvLine:string) = 
  // Create a list with individual cells
  let cells = List.ofSeq(csvLine.Split(','))
  
  // List should contain at least two elements
  match cells with
  | title::numericValue::_ ->
      // Convert second element to number & return tuple
      let parsedNumber = Int32.Parse(numericValue)
      (title, parsedNumber)            
  | _ -> failwith "Incorrect data format!"

// Listing 4.3: Parsing multiple lines from the input file

// Converts the data set loaded from CSV - takes a list 
// of strings and returns list of (lbl, num) tuples
let rec processLines(lines) =
  match lines with
  | [] -> []
  | currentLine::remaining -> 
      // For a non-empty list - 
      // process the first element & the rest recursively
      let parsedLine = convertDataRow(currentLine)
      let parsedRest = processLines(remaining)
      parsedLine :: parsedRest

// Listing 4.4 Calculating a sum of numeric values in the list

// Counts summary of values in the data set
let rec calculateSum(rows) = 
  match rows with
  | [] -> 0
  | (_, value)::tail -> 
      let remainingSum = calculateSum(tail)
      value + remainingSum

// ----------------------------------------------------------------------------
// Text version

// Section 4.3: Creating a console version

// Listing 4.5: Putting the console-based version together
// Prints data loaded from a specified CSV file
let writeGraph(file) = 
  // Load file as a list of strings (lines)
  let lines = List.ofSeq(File.ReadAllLines(file))

  // Convert lines to list of (lable, value) tuples 
  let data = processLines(lines)
  // Calculate summary of all values
  let sum = float(calculateSum(data))
  
  // Iterate over all the elements
  for (title, value) in data do
    // Calculate % of the current element & print it
    let percentage = int(float(value) / sum * 100.0)
    Console.WriteLine("{0,-18} - {1,8} ({2}%)", title, value, percentage)

do
  // Run the program & wait for input
  writeGraph "..\\..\\population_2000.csv"
  
  // Uncomment the following line to wait for [Enter] when running console-version
  // ignore(Console.ReadLine())

// ----------------------------------------------------------------------------
// Graphical version - GUI Using Windows Forms

// Section 4.4: Creating a Windows Forms application

// Listing 4.6: Building the user interface (F#)
open System.Drawing
open System.Windows.Forms

// Create main form window
let mainForm = new Form(Width = 620, Height = 450, Text = "Pie Chart")

// Create the menu with two items (Open and Save)
let menu = new ToolStrip()
let btnOpen = new ToolStripButton("Open")
let btnSave = new ToolStripButton("Save", Enabled = false)

ignore(menu.Items.Add(btnOpen))
ignore(menu.Items.Add(btnSave))
mainForm.Controls.Add(menu)

// Create picture box for displaying the chart
let boxChart = 
  new PictureBox
    (BackColor = Color.White, Dock = DockStyle.Fill,
     SizeMode = PictureBoxSizeMode.CenterImage)
mainForm.Controls.Add(boxChart)


// ----------------------------------------------------------------------------
// Graphical version - Drawing using System.Drawing

// Listing 4.7: Creating a brush with random colors

let rnd = new Random()

// Returns a brush of random color (generated using a 
// global 'rnd'). Calling 'rnd.Next' has side effects
let randomBrush() =
  let r, g, b = rnd.Next(256), rnd.Next(256), rnd.Next(256)
  new SolidBrush(Color.FromArgb(r,g,b))

// Listing 4.8: Drawing a segment of the pie chart

// Draws a part of the pie chart (specified by startAngle 
// & occupiedAngle) on the graphics 'gr' (label is ignored)
let drawPieSegment(gr:Graphics, title, startAngle, occupiedAngle) =
  let br = randomBrush()
  gr.FillPie(br, 170, 70, 260, 260, startAngle, occupiedAngle)
  br.Dispose()


// Listing 4.11: Drawing text labels
let fnt = new Font("Times New Roman", 11.0f)

let centerX, centerY = 300.0, 200.0
let labelDistance = 150.0

// Draws a label for a part of pie graph 
// The signature is same as a signature of 'drawPie'
let drawLabel(gr:Graphics, title, startAngle, occupiedAngle) =  
  // Calculate angle, where the text should be (in radians)
  let ra = (Math.PI * 2.0 * float((startAngle + occupiedAngle/2)) / 360.0)
  // ... and position for the center of the text
  let x = centerX + labelDistance * cos(ra)
  let y = centerY + labelDistance * sin(ra)

  // Get rectangle, in which the text will be drawn & draw it
  let size = gr.MeasureString(title, fnt)
  let rc = new PointF(float32(x) - size.Width / 2.0f, 
                      float32(y) - size.Height / 2.0f)
  gr.DrawString(title, fnt, Brushes.Black, new RectangleF(rc, size))


// Listing 4.9: Drawing items using specified drawing function

let drawStep(drawingFunc, gr:Graphics, sum, data) = 
   // Nested recursive function that processes the data
   let rec drawStepUtil(data, angleSoFar) =
      match data with 
      // Matches an empty list 
      | [] -> () 
      
      // Matches a list with a single element
      | [title, value] ->
         // Calculate the angle to add up to 360 
         let angle = 360 - angleSoFar
         // Draw the segment
         drawingFunc(gr, title, angleSoFar, angle) 
         
      // Matches a list with non-empty tail
      | (title, value)::tail ->
         let angle = int(float(value) / sum * 360.0) 
         // Draw the segment
         drawingFunc(gr, title, angleSoFar, angle) 
         // Recursively draw the rest
         drawStepUtil(tail, angleSoFar + angle)
   
   // Run the local utility function
   drawStepUtil(data, 0) 


// Listing 4.10: Drawing the chart

// Load data from the specified file, process the data and draw the chart
let drawChart(file) = 
  // Load file, convert it to list of tuples and calculate summary
  let lines = List.ofSeq(File.ReadAllLines(file))
  let data = processLines lines
  let sum = float(calculateSum(data))
  
  // Create a bitmap and object for drawing on it
  let pieChart = new Bitmap(600, 400)
  let gr = Graphics.FromImage(pieChart)

  // Fill with white color
  // Draw chart using two phases - first the chart then the labels
  gr.Clear(Color.White)
  drawStep(drawPieSegment, gr, sum, data)
  drawStep(drawLabel, gr, sum, data)

  // Finalize drawing & return bitmap
  gr.Dispose()
  pieChart


// Listing 4.12: Adding user interaction
    
// 'Open' button handler - show dialog, draw the image and display it
let openAndDrawChart(e) =
  let dlg = new OpenFileDialog(Filter="CSV Files|*.csv")
  if (dlg.ShowDialog() = DialogResult.OK) then
    let pieChart = drawChart dlg.FileName
    boxChart.Image <- pieChart
    btnSave.Enabled <- true

// 'Save' button handler - show dialog & save currently generated image    
let saveDrawing(e) = 
  let dlg = new SaveFileDialog(Filter="PNG Files|*.png")
  if (dlg.ShowDialog() = DialogResult.OK) then
    boxChart.Image.Save(dlg.FileName)

// Entry-point for the graphical version   
[<STAThread>]
do
  // Specify event handlers - a function called when button is clicked
  btnOpen.Click.Add(openAndDrawChart)
  btnSave.Click.Add(saveDrawing)
  Application.EnableVisualStyles()
  Application.Run(mainForm)