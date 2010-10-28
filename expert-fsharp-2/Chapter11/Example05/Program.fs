// Expert F# 2.0
// Chapter 11 Example 05

open System
open System.Threading
open System.Numerics
open System.Drawing
open System.Windows.Forms
open System.Xml

let sqrMod (x:Complex) = x.Real * x.Real + x.Imaginary * x.Imaginary
let rec mandel maxit (z:Complex) (c: Complex) count =
    if (sqrMod(z) < 4.0) &&  (count < maxit) then
        mandel maxit ((z * z) + c) c (count + 1)
    else count

let RGBtoHSV (r, g, b) =
    let (m:float) = min r (min g b)
    let (M:float) = max r (max g b)
    let delta = M - m
    let posh (h:float) = if h < 0.0 then h + 360.0 else h
    let deltaf (f:float) (s:float) = (f - s) / delta
    if M = 0.0 then (-1.0, 0.0, M) else
        let s = (M - m) / M
        if r = M then (posh(60.0 * (deltaf g b)), s, M)
        elif g = M then (posh(60.0 * (2.0 + (deltaf b r))), s, M)
        else (posh(60.0 * (4.0 + (deltaf r g))), s, M)

let HSVtoRGB (h, s, v) =
    if s = 0.0 then (v, v, v) else
    let hs = h / 60.0
    let i = floor (hs)
    let f = hs - i
    let p = v * ( 1.0 - s )
    let q = v * ( 1.0 - s * f )
    let t = v * ( 1.0 - s * ( 1.0 - f ))
    match int i with
      | 0 -> (v, t, p)
      | 1 -> (q, v, p)
      | 2 -> (p, v, t)
      | 3 -> (p, q, v)
      | 4 -> (t, p, v)
      | _ -> (v, p, q)

let makeColor (r, g, b) =
    Color.FromArgb(int32(r * 255.0), int32(g * 255.0), int32(b * 255.0))

let defaultColor i = makeColor(HSVtoRGB(360.0 * (float i / 250.0), 1.0, 1.0))

let coloring =
    [| defaultColor;
       (fun i -> Color.FromArgb(i, i, i));
       (fun i -> Color.FromArgb(i, 0, 0));
       (fun i -> Color.FromArgb(0, i, 0));
       (fun i -> Color.FromArgb(0, 0, i));
       (fun i -> Color.FromArgb(i, i, 0));
       (fun i -> Color.FromArgb(i, 250 - i, 0));
       (fun i -> Color.FromArgb(250 - i, i, i));
       (fun i -> if i % 2 = 0 then Color.White else Color.Black);
       (fun i -> Color.FromArgb(250 - i, 250 - i, 250 - i))
    |]

let createPalette c =
    Array.init 253 (function
        | 250 -> Color.Black
        | 251 -> Color.White
        | 252 -> Color.LightGray
        | i ->   c i)


let mutable palette = createPalette coloring.[0]

type CanvasForm() as x =
    inherit Form()
    do x.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
    override x.OnPaintBackground(args) = ()

// Creates the Form
let form = new CanvasForm(Width=800, Height=600,Text="Mandelbrot set")

let mutable worker = Thread.CurrentThread


let mutable startsel = Point.Empty
let mutable rect = Rectangle.Empty

let mutable tl = (-3.0, 2.0)
let mutable br = (2.0, -2.0)

let mutable menuIterations = 150

let iterations (tlx, tly) (brx, bry) =
    menuIterations

let mutable bitmap = new Bitmap(form.Width, form.Height)
let mutable bmpw = form.Width
let mutable bmph = form.Height

let timer = new Timer(Interval=100)
timer.Tick.Add(fun _ -> form.Invalidate() )

let pickColor maxit it =
    palette.[int(250.0 * float it / float maxit)]

let run filler (form:#Form) (bitmap:Bitmap) (tlx, tly) (brx, bry) =
    let dx = (brx - tlx) / float bmpw
    let dy = (tly - bry) / float bmph
    let maxit = iterations (tlx, tly) (brx, bry)
    let x = 0
    let y = 0
    let transform x y = new Complex(tlx + (float x) * dx, tly - (float y) * dy )
    form.Invoke(new MethodInvoker(fun () ->
        form.Text <- sprintf "Mandelbrot set [it: %d] (%f, %f) -> (%f, %f)"
                     maxit tlx tly brx bry
    )) |> ignore
    filler maxit transform
    timer.Enabled <- false

let linearFill (bw:int) (bh:int) maxit map =
    for y = 0 to bh - 1 do
        for x = 0 to bw - 1 do
            let c = mandel maxit Complex.Zero (map x y) 0
            lock bitmap (fun () -> bitmap.SetPixel(x, y, pickColor maxit c))


let blockFill (bw:int) (bh:int) maxit map =
    let rec fillBlock first sz x y =
        if x < bw then
            let c = mandel maxit Complex.Zero (map x y) 0
            lock bitmap (fun () ->
               let g = Graphics.FromImage(bitmap)
               g.FillRectangle(new SolidBrush(pickColor maxit c), x, y, sz, sz)
               g.Dispose()
            )
            fillBlock first sz (if first || ((y / sz) % 2 = 1) then x + sz
                                else x + 2 * sz) y
        elif y < bh then
            fillBlock first sz (if first || ((y / sz) % 2 = 0) then 0 else sz)
                         (y + sz)
        elif sz > 1 then
            fillBlock false (sz / 2) (sz / 2) 0

    fillBlock true 64 0 0

let mutable fillFun = blockFill

let clearOffScreen (b : Bitmap) =
    use g = Graphics.FromImage(b)
    g.Clear(Color.White)

let paint (g: Graphics) =
    lock bitmap (fun () -> g.DrawImage(bitmap, 0, 0))
    g.DrawRectangle(Pens.Black, rect)
    g.FillRectangle(new SolidBrush(Color.FromArgb(128, Color.White)), rect)

let stopWorker () =
    if worker <> Thread.CurrentThread then
        worker.Abort()
        worker <- Thread.CurrentThread

let drawMandel () =
    let bf = fillFun bmpw bmph
    stopWorker();
    timer.Enabled <- true
    worker <- new Thread(fun () -> run bf form bitmap tl br)
    worker.IsBackground <- true
    worker.Priority <- ThreadPriority.Lowest
    worker.Start()

let setCoord (tlx:float, tly:float) (brx:float, bry:float)  =
    let dx = (brx - tlx) / float bmpw
    let dy = (tly - bry) / float bmph
    let mapx x = tlx + float x * dx
    let mapy y = tly - float y * dy
    tl <- (mapx rect.Left, mapy rect.Top)
    br <- (mapx rect.Right, mapy rect.Bottom)

let ensureAspectRatio (tlx:float, tly:float) (brx:float, bry:float) =
    let ratio = (float bmpw / float bmph)
    let w, h = abs(brx - tlx), abs(tly - bry)
    if ratio * h > w then
        br <- (tlx + h * ratio, bry)
    else
        br <- (brx,tly - w / ratio)

let updateView () =
    if rect <> Rectangle.Empty then setCoord tl br
    ensureAspectRatio tl br
    rect <- Rectangle.Empty
    stopWorker()
    clearOffScreen bitmap
    drawMandel()
let click (arg:MouseEventArgs) =
    if rect.Contains(arg.Location) then
        updateView()
    else
        form.Invalidate()
        rect <- Rectangle.Empty
        startsel <- arg.Location

let mouseMove (arg:MouseEventArgs) =
    if arg.Button = MouseButtons.Left then
        let tlx = min startsel.X arg.X
        let tly = min startsel.Y arg.Y
        let brx = max startsel.X arg.X
        let bry = max startsel.Y arg.Y
        rect <- new Rectangle(tlx, tly, brx - tlx, bry - tly)
        form.Invalidate()

let resize () =
    if bmpw <> form.ClientSize.Width ||
       bmph <> form.ClientSize.Height then
         stopWorker()
         rect <- form.ClientRectangle
         bitmap <- new Bitmap(form.ClientSize.Width, form.ClientSize.Height)
         bmpw <- form.ClientSize.Width
         bmph <- form.ClientSize.Height

         updateView()

let zoom amount (tlx, tly) (brx, bry) =
    let w, h = abs(brx - tlx), abs(tly - bry)
    let nw, nh = amount * w, amount * h
    tl <- (tlx + (w - nw) / 2., tly - (h - nh) / 2.)
    br <- (brx - (w - nw) / 2., bry + (h - nh) / 2.)
    rect <- Rectangle.Empty
    updateView()

let selectDropDownItem (l:ToolStripMenuItem) (o:ToolStripMenuItem) =
    for el in l.DropDownItems do
      let item = (el :?> ToolStripMenuItem)
      item.Checked <- (o = item)
let setFillMode (p:ToolStripMenuItem) (m:ToolStripMenuItem) filler _ =
    if (not m.Checked) then
        selectDropDownItem p m
        fillFun <- filler
        drawMandel()


let setupMenu () =
  let m = new MenuStrip()
  let f = new ToolStripMenuItem("&File")
  let c = new ToolStripMenuItem("&Settings")
  let e = new ToolStripMenuItem("&Edit")
  let ext = new ToolStripMenuItem("E&xit")
  let cols = new ToolStripComboBox("ColorScheme")
  let its = new ToolStripComboBox("Iterations")
  let copybmp = new ToolStripMenuItem("Copy &bitmap")
  let copy = new ToolStripMenuItem("&Copy")
  let paste = new ToolStripMenuItem("&Paste")
  let zoomin = new ToolStripMenuItem("Zoom &In")
  let zoomout = new ToolStripMenuItem("Zoom &Out")
  let fillMode = new ToolStripMenuItem("Fill mode")
  let fillModeLinear = new ToolStripMenuItem("Line")
  let fillModeBlock = new ToolStripMenuItem("Block")

  let itchg = fun _ ->
    menuIterations <- System.Int32.Parse(its.Text)
    stopWorker()
    drawMandel()
    c.HideDropDown()
  ext.Click.Add(fun _ -> form.Dispose()) |> ignore

  copybmp.Click.Add(fun _ -> Clipboard.SetDataObject(bitmap))|> ignore
  copybmp.ShortcutKeyDisplayString <- "Ctrl+Shift+C"
  copybmp.ShortcutKeys <- Keys.Control ||| Keys.Shift ||| Keys.C

  copy.Click.Add(fun _ ->
      let maxit = (iterations tl br)
      let tlx, tly = tl
      let brx, bry = br
      Clipboard.SetText(sprintf "<Mandel iter=\"%d\"><topleft><re>%.14e</re><im>%.14e</im></topleft><bottomright><re>%.14e</re><im>%.14e</im></bottomright></Mandel>" maxit tlx tly brx bry)
  ) |> ignore
  copy.ShortcutKeyDisplayString <- "Ctrl+C"
  copy.ShortcutKeys <- Keys.Control ||| Keys.C

  paste.Click.Add(fun _ ->
      if Clipboard.ContainsText() then
        let doc = new XmlDocument()
        try
          doc.LoadXml(Clipboard.GetText())
          menuIterations <- int (doc.SelectSingleNode("/Mandel").Attributes.["iter"].Value)
          tl <- (float (doc.SelectSingleNode("//topleft/re").InnerText), float (doc.SelectSingleNode("//topleft/im").InnerText))
          br <- (float (doc.SelectSingleNode("//bottomright/re").InnerText), float (doc.SelectSingleNode("//bottomright/im").InnerText))
          rect <- Rectangle.Empty
          updateView()
        with _ -> ()
  ) |> ignore
  paste.ShortcutKeyDisplayString <- "Ctrl+V"
  paste.ShortcutKeys <- Keys.Control ||| Keys.V
  zoomin.Click.Add(fun _ -> zoom 0.9 tl br) |> ignore
  zoomin.ShortcutKeyDisplayString <- "Ctrl+T"
  zoomin.ShortcutKeys <- Keys.Control ||| Keys.T
  zoomout.Click.Add(fun _ -> zoom 1.25 tl br) |> ignore
  zoomout.ShortcutKeyDisplayString <- "Ctrl+W"
  zoomout.ShortcutKeys <- Keys.Control ||| Keys.W

  for x in [ f;e;c ] do m.Items.Add(x) |> ignore
  f.DropDownItems.Add(ext) |> ignore
  let tsi x = (x :> ToolStripItem)
  for x in [ tsi cols; tsi its; tsi fillMode] do c.DropDownItems.Add(x) |> ignore
  for x in [ tsi copy; tsi zoomin; tsi zoomout ] do e.DropDownItems.Add(x) |> ignore
  for x in ["HSL Color"; "Gray"; "Red"; "Green"] do cols.Items.Add(x) |> ignore
  fillMode.DropDownItems.Add(fillModeLinear) |> ignore
  fillMode.DropDownItems.Add(fillModeBlock) |> ignore
  cols.SelectedIndex <- 0
  cols.DropDownStyle <- ComboBoxStyle.DropDownList

  cols.SelectedIndexChanged.Add(fun _ ->
    palette <- createPalette coloring.[cols.SelectedIndex]
    stopWorker()
    drawMandel()
    c.HideDropDown()
  )
  its.Text <- menuIterations.ToString()
  its.DropDownStyle <- ComboBoxStyle.DropDown
  for x in [ "150"; "250"; "500"; "1000" ] do its.Items.Add(x) |> ignore
  its.LostFocus.Add(itchg)
  its.SelectedIndexChanged.Add(itchg)
  fillModeBlock.Checked <- true
  fillModeLinear.Click.Add(setFillMode fillMode fillModeLinear linearFill)
  fillModeBlock.Click.Add(setFillMode fillMode fillModeBlock blockFill)
  m

clearOffScreen bitmap
form.MainMenuStrip <- setupMenu()
form.Controls.Add(form.MainMenuStrip)
form.MainMenuStrip.RenderMode <- ToolStripRenderMode.System
form.Paint.Add(fun arg ->  paint arg.Graphics)
form.MouseDown.Add(click)
form.MouseMove.Add(mouseMove)
form.ResizeEnd.Add(fun _ -> resize())
form.Show()

Application.DoEvents()

drawMandel()

[<STAThread>]
do Application.Run(form)
