namespace GraphicalFilters_CSharp
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
      this.toolStrip1 = new System.Windows.Forms.ToolStrip();
      this.btnOpen = new System.Windows.Forms.ToolStripButton();
      this.btnUseProcessed = new System.Windows.Forms.ToolStripButton();
      this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
      this.toolStripLabel1 = new System.Windows.Forms.ToolStripLabel();
      this.listFilters = new System.Windows.Forms.ToolStripComboBox();
      this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
      this.btnSequential = new System.Windows.Forms.ToolStripButton();
      this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
      this.lblTime = new System.Windows.Forms.ToolStripLabel();
      this.tabControl1 = new System.Windows.Forms.TabControl();
      this.tabPage1 = new System.Windows.Forms.TabPage();
      this.pictOriginal = new System.Windows.Forms.PictureBox();
      this.tabPage2 = new System.Windows.Forms.TabPage();
      this.pictProcessed = new System.Windows.Forms.PictureBox();
      this.openImageDlg = new System.Windows.Forms.OpenFileDialog();
      this.toolStrip1.SuspendLayout();
      this.tabControl1.SuspendLayout();
      this.tabPage1.SuspendLayout();
      ((System.ComponentModel.ISupportInitialize)(this.pictOriginal)).BeginInit();
      this.tabPage2.SuspendLayout();
      ((System.ComponentModel.ISupportInitialize)(this.pictProcessed)).BeginInit();
      this.SuspendLayout();
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
            this.btnSequential,
            this.toolStripSeparator3,
            this.lblTime});
      this.toolStrip1.Location = new System.Drawing.Point(0, 0);
      this.toolStrip1.Name = "toolStrip1";
      this.toolStrip1.Size = new System.Drawing.Size(663, 25);
      this.toolStrip1.TabIndex = 0;
      this.toolStrip1.Text = "toolStrip1";
      // 
      // btnOpen
      // 
      this.btnOpen.Image = ((System.Drawing.Image)(resources.GetObject("btnOpen.Image")));
      this.btnOpen.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.btnOpen.Name = "btnOpen";
      this.btnOpen.Size = new System.Drawing.Size(101, 22);
      this.btnOpen.Text = "Open image...";
      this.btnOpen.Click += new System.EventHandler(this.btnOpen_Click);
      // 
      // btnUseProcessed
      // 
      this.btnUseProcessed.Image = ((System.Drawing.Image)(resources.GetObject("btnUseProcessed.Image")));
      this.btnUseProcessed.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.btnUseProcessed.Name = "btnUseProcessed";
      this.btnUseProcessed.Size = new System.Drawing.Size(102, 22);
      this.btnUseProcessed.Text = "Use processed";
      this.btnUseProcessed.Click += new System.EventHandler(this.btnUseProcessed_Click);
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
      // btnSequential
      // 
      this.btnSequential.Image = ((System.Drawing.Image)(resources.GetObject("btnSequential.Image")));
      this.btnSequential.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.btnSequential.Name = "btnSequential";
      this.btnSequential.Size = new System.Drawing.Size(58, 22);
      this.btnSequential.Text = "Apply";
      this.btnSequential.Click += new System.EventHandler(this.btnRun_Click);
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
      // tabControl1
      // 
      this.tabControl1.Controls.Add(this.tabPage1);
      this.tabControl1.Controls.Add(this.tabPage2);
      this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
      this.tabControl1.Location = new System.Drawing.Point(0, 25);
      this.tabControl1.Name = "tabControl1";
      this.tabControl1.SelectedIndex = 0;
      this.tabControl1.Size = new System.Drawing.Size(663, 331);
      this.tabControl1.TabIndex = 1;
      // 
      // tabPage1
      // 
      this.tabPage1.Controls.Add(this.pictOriginal);
      this.tabPage1.Location = new System.Drawing.Point(4, 22);
      this.tabPage1.Name = "tabPage1";
      this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
      this.tabPage1.Size = new System.Drawing.Size(655, 305);
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
      this.pictOriginal.Size = new System.Drawing.Size(649, 299);
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
      this.tabPage2.Size = new System.Drawing.Size(655, 305);
      this.tabPage2.TabIndex = 1;
      this.tabPage2.Text = "Processed";
      this.tabPage2.UseVisualStyleBackColor = true;
      // 
      // pictProcessed
      // 
      this.pictProcessed.Dock = System.Windows.Forms.DockStyle.Fill;
      this.pictProcessed.Location = new System.Drawing.Point(3, 3);
      this.pictProcessed.Name = "pictProcessed";
      this.pictProcessed.Size = new System.Drawing.Size(649, 299);
      this.pictProcessed.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
      this.pictProcessed.TabIndex = 0;
      this.pictProcessed.TabStop = false;
      // 
      // openImageDlg
      // 
      this.openImageDlg.Filter = "All images|*.jpg;*.png;*.bmp;*.gif";
      // 
      // MainForm
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(663, 356);
      this.Controls.Add(this.tabControl1);
      this.Controls.Add(this.toolStrip1);
      this.Name = "MainForm";
      this.Text = "Image Filters";
      this.toolStrip1.ResumeLayout(false);
      this.toolStrip1.PerformLayout();
      this.tabControl1.ResumeLayout(false);
      this.tabPage1.ResumeLayout(false);
      ((System.ComponentModel.ISupportInitialize)(this.pictOriginal)).EndInit();
      this.tabPage2.ResumeLayout(false);
      ((System.ComponentModel.ISupportInitialize)(this.pictProcessed)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.ToolStrip toolStrip1;
		private System.Windows.Forms.ToolStripButton btnUseProcessed;
		private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
		private System.Windows.Forms.ToolStripComboBox listFilters;
    private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
		private System.Windows.Forms.ToolStripButton btnSequential;
		private System.Windows.Forms.ToolStripLabel toolStripLabel1;
		private System.Windows.Forms.TabControl tabControl1;
		private System.Windows.Forms.TabPage tabPage1;
		private System.Windows.Forms.PictureBox pictOriginal;
		private System.Windows.Forms.TabPage tabPage2;
		private System.Windows.Forms.PictureBox pictProcessed;
		private System.Windows.Forms.OpenFileDialog openImageDlg;
		private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
		private System.Windows.Forms.ToolStripLabel lblTime;
		private System.Windows.Forms.ToolStripButton btnOpen;
	}
}

