<%@ Page Language="F#" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
                      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script language="F#" runat="server">
    member page.GenerateData_Click(sender: obj, e: EventArgs) =
        let isPrime(i: int) =
            let lim = int(sqrt(float(i)))
            let rec check j =
                j > lim or (i % j <> 0 && check (j+1))
            check 2

        let lowerLimit = System.Convert.ToInt32(page.LowerLimit.Text)
        let upperLimit = System.Convert.ToInt32(page.UpperLimit.Text)
        let data =
            [ let previousTime = ref System.DateTime.Now
              for i in lowerLimit..upperLimit do
                  if isPrime(i) then
                      let time = System.DateTime.Now
                      yield (i, time-previousTime.Value)
                      do previousTime := time ]
        page.Repeater.DataSource <- data
        page.Repeater.DataBind()
</script>

<html xmlns="http://www.w3.org/1999/xhtml">
<head id="Head1" runat="server">
    <title>Current time</title>
    <style type="text/css">
        body { font-family:calibri,verdana,sans-serif; }
    </style>
</head>
<body>
    <form id="Form1" runat="server">
        <h2>Displaying data</h2>
        <p>
            Compute primes
            from <asp:TextBox runat="server" id="LowerLimit" />
            to   <asp:TextBox runat="server" id="UpperLimit" />.
        </p>
        <asp:Button runat="server"
                    id="GenerateData"
                    text="Generate" OnClick="GenerateData_Click" />
        <p>
        Results:

        <ul>
        <asp:Repeater id="Repeater" runat="server">
            <ItemTemplate>
                <li style="color:blue">
                    n = <%# this.Eval("Item1") %>,
                    time since previous: <%# this.Eval("Item2") %></li>
            </ItemTemplate>
            <AlternatingItemTemplate>
                <li style="color:green">
                    n = <%# this.Eval("Item1") %>,
                    time since previous: <%# this.Eval("Item2") %></li>
            </AlternatingItemTemplate>
        </asp:Repeater>
        </ul>

        </p>
    </form>
</body>
</html>
