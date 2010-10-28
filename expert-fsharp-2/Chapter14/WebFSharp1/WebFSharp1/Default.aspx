<%@ Page Language="F#" %>
<html>
<head id="Head1" runat="server">
    <title>Products in a Category</title>
</head>
<body>
    <form id="Form1" runat="server">
        <!-- Unordered list of products using ASP.NET Repeater -->
        <ul>
        <asp:Repeater runat="server" id="rptProducts"
                      DataSourceID="awProducts">
            <ItemTemplate>
                <li><%# this.Eval("Name") %>
                     (price: <%# this.Eval("Price") %>)</li>
            </ItemTemplate>
        </asp:Repeater>
        </ul>

        <!-- ASP.NET DataSource control for loading the data -->
        <asp:ObjectDataSource id="awProducts" runat="server"
            TypeName="FSharpWeb.DataSource" SelectMethod="GetProducts">
            <SelectParameters>
                <asp:QueryStringParameter Name="categoryId" Type="Int32"
                     QueryStringField="id" DefaultValue="20"/>
            </SelectParameters>
        </asp:ObjectDataSource>
    </form>
</body>
</html>
