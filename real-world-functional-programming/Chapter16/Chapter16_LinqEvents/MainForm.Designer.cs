namespace Chapter16_LinqEvents
{
  partial class MainForm
  {
    /// <summary>
    /// Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override void Dispose(bool disposing)
    {
      if (disposing && (components != null))
      {
        components.Dispose();
      }
      base.Dispose(disposing);
    }

    #region Windows Form Designer generated code

    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent()
    {
      this.btnUp = new System.Windows.Forms.Button();
      this.lblCount = new System.Windows.Forms.Label();
      this.btnDown = new System.Windows.Forms.Button();
      this.SuspendLayout();
      // 
      // btnUp
      // 
      this.btnUp.FlatStyle = System.Windows.Forms.FlatStyle.System;
      this.btnUp.Location = new System.Drawing.Point(12, 12);
      this.btnUp.Name = "btnUp";
      this.btnUp.Size = new System.Drawing.Size(110, 29);
      this.btnUp.TabIndex = 0;
      this.btnUp.Text = "Increment";
      this.btnUp.UseVisualStyleBackColor = true;
      // 
      // lblCount
      // 
      this.lblCount.AutoSize = true;
      this.lblCount.Font = new System.Drawing.Font("Calibri", 32.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
      this.lblCount.Location = new System.Drawing.Point(128, 17);
      this.lblCount.Name = "lblCount";
      this.lblCount.Size = new System.Drawing.Size(173, 53);
      this.lblCount.TabIndex = 1;
      this.lblCount.Text = "Count: 0";
      // 
      // btnDown
      // 
      this.btnDown.FlatStyle = System.Windows.Forms.FlatStyle.System;
      this.btnDown.Location = new System.Drawing.Point(12, 47);
      this.btnDown.Name = "btnDown";
      this.btnDown.Size = new System.Drawing.Size(110, 29);
      this.btnDown.TabIndex = 2;
      this.btnDown.Text = "Decrement";
      this.btnDown.UseVisualStyleBackColor = true;
      // 
      // MainForm
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(317, 95);
      this.Controls.Add(this.btnDown);
      this.Controls.Add(this.lblCount);
      this.Controls.Add(this.btnUp);
      this.Name = "MainForm";
      this.Text = "MainForm";
      this.Load += new System.EventHandler(this.MainForm_Load);
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Button btnUp;
    private System.Windows.Forms.Label lblCount;
    private System.Windows.Forms.Button btnDown;
  }
}