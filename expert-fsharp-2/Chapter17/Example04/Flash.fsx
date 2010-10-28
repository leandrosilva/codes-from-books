#r "AxShockwaveFlashObjects.dll"

open AxShockwaveFlashObjects
open System.Windows.Forms
let f = new Form()
let flash = new AxShockwaveFlash()
f.Show()
flash.Dock <- DockStyle.Fill
f.Controls.Add(flash)
flash.LoadMovie(0, "http://laptop.org/img/meshDemo18.swf")

