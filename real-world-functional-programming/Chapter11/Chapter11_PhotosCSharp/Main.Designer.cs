namespace Chapter11_PhotoApp_CSharp
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
			this.list = new System.Windows.Forms.ListBox();
			this.pict = new System.Windows.Forms.PictureBox();
			((System.ComponentModel.ISupportInitialize)(this.pict)).BeginInit();
			this.SuspendLayout();
			// 
			// list
			// 
			this.list.FormattingEnabled = true;
			this.list.Location = new System.Drawing.Point(12, 12);
			this.list.Name = "list";
			this.list.Size = new System.Drawing.Size(200, 303);
			this.list.TabIndex = 0;
			// 
			// pict
			// 
			this.pict.Location = new System.Drawing.Point(218, 12);
			this.pict.Name = "pict";
			this.pict.Size = new System.Drawing.Size(400, 300);
			this.pict.TabIndex = 1;
			this.pict.TabStop = false;
			// 
			// Form1
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(625, 319);
			this.Controls.Add(this.pict);
			this.Controls.Add(this.list);
			this.Name = "Form1";
			this.Text = "Photos";
			this.Load += new System.EventHandler(this.Form1_Load);
			((System.ComponentModel.ISupportInitialize)(this.pict)).EndInit();
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.ListBox list;
		private System.Windows.Forms.PictureBox pict;
	}
}

