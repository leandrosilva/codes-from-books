namespace Chapter14_FSharpEffects.Forms
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
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
      this.tabControl1 = new System.Windows.Forms.TabControl();
      this.tabPage1 = new System.Windows.Forms.TabPage();
      this.pictOriginal = new System.Windows.Forms.PictureBox();
      this.tabPage2 = new System.Windows.Forms.TabPage();
      this.pictProcessed = new System.Windows.Forms.PictureBox();
      this.toolStrip1 = new System.Windows.Forms.ToolStrip();
      this.btnOpen = new System.Windows.Forms.ToolStripButton();
      this.btnUseProcessed = new System.Windows.Forms.ToolStripButton();
      this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
      this.toolStripLabel1 = new System.Windows.Forms.ToolStripLabel();
      this.listFilters = new System.Windows.Forms.ToolStripComboBox();
      this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
      this.btnApply = new System.Windows.Forms.ToolStripButton();
      this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
      this.lblTime = new System.Windows.Forms.ToolStripLabel();
      this.openImageDlg = new System.Windows.Forms.OpenFileDialog();
      this.tabControl1.SuspendLayout();
      this.tabPage1.SuspendLayout();
      ((System.ComponentModel.ISupportInitialize)(this.pictOriginal)).BeginInit();
      this.tabPage2.SuspendLayout();
      ((System.ComponentModel.ISupportInitialize)(this.pictProcessed)).BeginInit();
      this.toolStrip1.SuspendLayout();
      this.SuspendLayout();
      // 
      // tabControl1
      // 
      this.tabControl1.Controls.Add(this.tabPage1);
      this.tabControl1.Controls.Add(this.tabPage2);
      this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
      this.tabControl1.Location = new System.Drawing.Point(0, 25);
      this.tabControl1.Name = "tabControl1";
      this.tabControl1.SelectedIndex = 0;
      this.tabControl1.Size = new System.Drawing.Size(633, 357);
      this.tabControl1.TabIndex = 3;
      // 
      // tabPage1
      // 
      this.tabPage1.Controls.Add(this.pictOriginal);
      this.tabPage1.Location = new System.Drawing.Point(4, 22);
      this.tabPage1.Name = "tabPage1";
      this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
      this.tabPage1.Size = new System.Drawing.Size(625, 331);
      this.tabPage1.TabIndex = 0;
      this.tabPage1.Text = "Original";
      this.tabPage1.UseVisualStyleBackColor = true;
      // 
      // pictOriginal
      // 
      this.pictOriginal.BackColor = System.Drawing.Color.Transparent;
      this.pictOriginal.Dock = System.Windows.Forms.DockStyle.Fill;
      this.pictOriginal.Location = new System.Drawing.Point(3, 3);
      this.pictOriginal.Name = "pictOriginal";
      this.pictOriginal.Size = new System.Drawing.Size(619, 325);
      this.pictOriginal.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
      this.pictOriginal.TabIndex = 1;
      this.pictOriginal.TabStop = false;
      // 
      // tabPage2
      // 
      this.tabPage2.Controls.Add(this.pictProcessed);
      this.tabPage2.Location = new System.Drawing.Point(4, 22);
      this.tabPage2.Name = "tabPage2";
      this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
      this.tabPage2.Size = new System.Drawing.Size(625, 331);
      this.tabPage2.TabIndex = 1;
      this.tabPage2.Text = "Processed";
      this.tabPage2.UseVisualStyleBackColor = true;
      // 
      // pictProcessed
      // 
      this.pictProcessed.Dock = System.Windows.Forms.DockStyle.Fill;
      this.pictProcessed.Location = new System.Drawing.Point(3, 3);
      this.pictProcessed.Name = "pictProcessed";
      this.pictProcessed.Size = new System.Drawing.Size(619, 325);
      this.pictProcessed.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
      this.pictProcessed.TabIndex = 0;
      this.pictProcessed.TabStop = false;
      // 
      // toolStrip1
      // 
      this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.btnOpen,
            this.btnUseProcessed,
            this.toolStripSeparator1,
            this.toolStripLabel1,
            this.listFilters,
            this.toolStripSeparator2,
            this.btnApply,
            this.toolStripSeparator3,
            this.lblTime});
      this.toolStrip1.Location = new System.Drawing.Point(0, 0);
      this.toolStrip1.Name = "toolStrip1";
      this.toolStrip1.Size = new System.Drawing.Size(633, 25);
      this.toolStrip1.TabIndex = 2;
      this.toolStrip1.Text = "toolStrip1";
      // 
      // btnOpen
      // 
      this.btnOpen.Image = ((System.Drawing.Image)(resources.GetObject("btnOpen.Image")));
      this.btnOpen.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.btnOpen.Name = "btnOpen";
      this.btnOpen.Size = new System.Drawing.Size(101, 22);
      this.btnOpen.Text = "Open image...";
      // 
      // btnUseProcessed
      // 
      this.btnUseProcessed.Image = ((System.Drawing.Image)(resources.GetObject("btnUseProcessed.Image")));
      this.btnUseProcessed.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.btnUseProcessed.Name = "btnUseProcessed";
      this.btnUseProcessed.Size = new System.Drawing.Size(102, 22);
      this.btnUseProcessed.Text = "Use processed";
      // 
      // toolStripSeparator1
      // 
      this.toolStripSeparator1.Name = "toolStripSeparator1";
      this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
      // 
      // toolStripLabel1
      // 
      this.toolStripLabel1.Name = "toolStripLabel1";
      this.toolStripLabel1.Size = new System.Drawing.Size(40, 22);
      this.toolStripLabel1.Text = "Effect:";
      // 
      // listFilters
      // 
      this.listFilters.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
      this.listFilters.Name = "listFilters";
      this.listFilters.Size = new System.Drawing.Size(200, 25);
      // 
      // toolStripSeparator2
      // 
      this.toolStripSeparator2.Name = "toolStripSeparator2";
      this.toolStripSeparator2.Size = new System.Drawing.Size(6, 25);
      // 
      // btnApply
      // 
      this.btnApply.Image = ((System.Drawing.Image)(resources.GetObject("btnApply.Image")));
      this.btnApply.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.btnApply.Name = "btnApply";
      this.btnApply.Size = new System.Drawing.Size(58, 22);
      this.btnApply.Text = "Apply";
      // 
      // toolStripSeparator3
      // 
      this.toolStripSeparator3.Name = "toolStripSeparator3";
      this.toolStripSeparator3.Size = new System.Drawing.Size(6, 25);
      // 
      // lblTime
      // 
      this.lblTime.Name = "lblTime";
      this.lblTime.Size = new System.Drawing.Size(62, 22);
      this.lblTime.Text = "Time: 0ms";
      // 
      // openImageDlg
      // 
      this.openImageDlg.Filter = "All images|*.jpg;*.png;*.bmp;*.gif";
      // 
      // MainForm
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(633, 382);
      this.Controls.Add(this.tabControl1);
      this.Controls.Add(this.toolStrip1);
      this.Name = "MainForm";
      this.Text = "Image Effects";
      this.tabControl1.ResumeLayout(false);
      this.tabPage1.ResumeLayout(false);
      ((System.ComponentModel.ISupportInitialize)(this.pictOriginal)).EndInit();
      this.tabPage2.ResumeLayout(false);
      ((System.ComponentModel.ISupportInitialize)(this.pictProcessed)).EndInit();
      this.toolStrip1.ResumeLayout(false);
      this.toolStrip1.PerformLayout();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    public System.Windows.Forms.TabControl tabControl1;
    public System.Windows.Forms.TabPage tabPage1;
    public System.Windows.Forms.PictureBox pictOriginal;
    public System.Windows.Forms.TabPage tabPage2;
    public System.Windows.Forms.PictureBox pictProcessed;
    public System.Windows.Forms.ToolStrip toolStrip1;
    public System.Windows.Forms.ToolStripButton btnOpen;
    public System.Windows.Forms.ToolStripButton btnUseProcessed;
    public System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
    public System.Windows.Forms.ToolStripLabel toolStripLabel1;
    public System.Windows.Forms.ToolStripComboBox listFilters;
    public System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
    public System.Windows.Forms.ToolStripButton btnApply;
    public System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
    public System.Windows.Forms.ToolStripLabel lblTime;
    public System.Windows.Forms.OpenFileDialog openImageDlg;

  }
}

