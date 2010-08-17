<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MainForm
  Inherits System.Windows.Forms.Form

  'Form overrides dispose to clean up the component list.
  <System.Diagnostics.DebuggerNonUserCode()> _
  Protected Overrides Sub Dispose(ByVal disposing As Boolean)
    Try
      If disposing AndAlso components IsNot Nothing Then
        components.Dispose()
      End If
    Finally
      MyBase.Dispose(disposing)
    End Try
  End Sub

  'Required by the Windows Form Designer
  Private components As System.ComponentModel.IContainer

  'NOTE: The following procedure is required by the Windows Form Designer
  'It can be modified using the Windows Form Designer.  
  'Do not modify it using the code editor.
  <System.Diagnostics.DebuggerStepThrough()> _
  Private Sub InitializeComponent()
    Me.btnDown = New System.Windows.Forms.Button
    Me.lblCount = New System.Windows.Forms.Label
    Me.btnUp = New System.Windows.Forms.Button
    Me.SuspendLayout()
    '
    'btnDown
    '
    Me.btnDown.FlatStyle = System.Windows.Forms.FlatStyle.System
    Me.btnDown.Location = New System.Drawing.Point(12, 47)
    Me.btnDown.Name = "btnDown"
    Me.btnDown.Size = New System.Drawing.Size(110, 29)
    Me.btnDown.TabIndex = 5
    Me.btnDown.Text = "Decrement"
    Me.btnDown.UseVisualStyleBackColor = True
    '
    'lblCount
    '
    Me.lblCount.AutoSize = True
    Me.lblCount.Font = New System.Drawing.Font("Calibri", 32.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
    Me.lblCount.Location = New System.Drawing.Point(128, 17)
    Me.lblCount.Name = "lblCount"
    Me.lblCount.Size = New System.Drawing.Size(173, 53)
    Me.lblCount.TabIndex = 4
    Me.lblCount.Text = "Count: 0"
    '
    'btnUp
    '
    Me.btnUp.FlatStyle = System.Windows.Forms.FlatStyle.System
    Me.btnUp.Location = New System.Drawing.Point(12, 12)
    Me.btnUp.Name = "btnUp"
    Me.btnUp.Size = New System.Drawing.Size(110, 29)
    Me.btnUp.TabIndex = 3
    Me.btnUp.Text = "Increment"
    Me.btnUp.UseVisualStyleBackColor = True
    '
    'MainForm
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(324, 89)
    Me.Controls.Add(Me.btnDown)
    Me.Controls.Add(Me.lblCount)
    Me.Controls.Add(Me.btnUp)
    Me.Name = "MainForm"
    Me.Text = "MainForm"
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub
  Private WithEvents btnDown As System.Windows.Forms.Button
  Private WithEvents lblCount As System.Windows.Forms.Label
  Private WithEvents btnUp As System.Windows.Forms.Button

End Class
