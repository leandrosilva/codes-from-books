namespace MyUserControl

open System
open System.Web.UI.WebControls

type RepeatText() =
    inherit System.Web.UI.UserControl()

    /// This is the internal state and parameters of the control
    let mutable text = ""
    let mutable count = 0

    /// This internal control is initialized by ASP.NET
    [<DefaultValue>]
    val mutable Place : Label


    /// These properties allow the state to be used from the page
    member self.Text        with get() = text  and set(v) = text <- v
    member self.RepeatCount with get() = count and set(v) = count <- v

    /// This event is automatically wired up through the use of the
    /// AutoEventWireup ASP.NET directive (true by default)
    member self.Page_Load (sender: obj, e: EventArgs) =
        let acc = new Text.StringBuilder()
        for i in 1..count do
            acc.Append(self.Text) |> ignore
        self.Place.Text <- acc.ToString()
