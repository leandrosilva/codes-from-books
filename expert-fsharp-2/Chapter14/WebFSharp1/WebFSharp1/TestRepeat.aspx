<%@ Page AutoEventWireup="true" %>
<%@ Register Src="RepeatText.ascx" TagName="Repeater" TagPrefix="text" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
                      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head id="Head1" runat="server">
    <title>My User Control Test</title>
</head>
<body>
    <form id="Form1" runat="server">
        <text:Repeater id="chart" runat="server" RepeatCount="10" Text="Monkey!" />
    </form>
</body>
</html>
