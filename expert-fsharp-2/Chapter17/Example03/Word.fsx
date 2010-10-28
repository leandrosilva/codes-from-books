open System

let o = Activator.CreateInstance(Type.GetTypeFromProgID("Word.Application"))
let t = o.GetType()

t.GetProperty("Visible").SetValue(o, (true :> Object), null)

let m = t.GetMethod("Quit")
m.GetParameters().Length

m.GetParameters()
m.Invoke(o, [| null; null; null |])
