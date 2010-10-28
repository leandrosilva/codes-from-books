// Expert F# 2.0
// Chapter 11 Example 03

open System
open System.Drawing
open System.Windows.Forms
open System.ComponentModel

type OwnerDrawButton() =
    inherit UserControl()

    let mutable text = ""
    let mutable pressed = false

    override x.OnPaint (e:PaintEventArgs) =
        let g = e.Graphics
        use pll = new Pen(SystemColors.ControlLightLight)
        use pl = new Pen(SystemColors.ControlLight)
        use pd = new Pen(SystemColors.ControlDark)
        use pdd = new Pen(SystemColors.ControlDarkDark)
        use bfg = new SolidBrush(x.ForeColor)
        let szf = g.MeasureString(text, x.Font)
        let spt = PointF((float32(x.Width) - szf.Width) / 2.0f,
                            (float32(x.Height) - szf.Height) / 2.0f)
        let ptt = if pressed then pdd else pll
        let pt = if pressed then pd else pl
        let pb = if pressed then pl else pd
        let pbb = if pressed then pll else pdd

        g.Clear(SystemColors.Control)
        g.DrawLine(ptt, 0, 0, x.Width - 1, 0)
        g.DrawLine(ptt, 0, 0, 0, x.Height - 1)
        g.DrawLine(pt, 1, 1, x.Width - 2, 1)
        g.DrawLine(pt, 1, 1, 1, x.Height - 2)
        g.DrawLine(pbb, 0, x.Height - 1, x.Width - 1, x.Height - 1)
        g.DrawLine(pbb, x.Width - 1, 0, x.Width - 1, x.Height - 1)
        g.DrawLine(pb, 1, x.Height - 2, x.Width - 2, x.Height - 2)
        g.DrawLine(pb, x.Width - 2, 1, x.Width - 2, x.Height - 2)
        g.DrawString(text, x.Font, bfg, spt)

    override x.OnMouseUp (e:MouseEventArgs) =
        pressed <- false
        x.Invalidate()

    override x.OnMouseDown (e:MouseEventArgs) =
        pressed <- true
        x.Invalidate()

    [<Category("Behavior")>]
    [<Browsable(true)>]
    override x.Text
        with get() = text
        and  set(t:string) = text <- t; x.Invalidate()

let form = new Form(Visible=true)
let c = new OwnerDrawButton(Text="Hello button")

c.Click.Add(fun _ -> MessageBox.Show("Clicked!") |> ignore)
form.Controls.Add(c)

[<STAThread>]
do Application.Run(form)