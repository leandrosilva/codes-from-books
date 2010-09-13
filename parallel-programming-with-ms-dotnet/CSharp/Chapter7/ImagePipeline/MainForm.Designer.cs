//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ImagePipeline
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
            if (cts != null)
            {
                cts.Dispose();
                cts = null;
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
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.quitButton = new System.Windows.Forms.Button();
            this.textBoxPhase1AvgTime = new System.Windows.Forms.TextBox();
            this.textBoxPhase2AvgTime = new System.Windows.Forms.TextBox();
            this.textBoxPhase3AvgTime = new System.Windows.Forms.TextBox();
            this.textBoxPhase4AvgTime = new System.Windows.Forms.TextBox();
            this.textBoxFileName = new System.Windows.Forms.TextBox();
            this.textBoxImageCount = new System.Windows.Forms.TextBox();
            this.textBoxFps = new System.Windows.Forms.TextBox();
            this.textBoxQueue1AvgWait = new System.Windows.Forms.TextBox();
            this.textBoxQueue2AvgWait = new System.Windows.Forms.TextBox();
            this.textBoxQueue3AvgWait = new System.Windows.Forms.TextBox();
            this.textBoxQueueCount1 = new System.Windows.Forms.TextBox();
            this.textBoxQueueCount2 = new System.Windows.Forms.TextBox();
            this.textBoxQueueCount3 = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.label11 = new System.Windows.Forms.Label();
            this.buttonStart = new System.Windows.Forms.Button();
            this.radioButtonSequential = new System.Windows.Forms.RadioButton();
            this.radioButtonPipeline = new System.Windows.Forms.RadioButton();
            this.radioButtonLoadBalanced = new System.Windows.Forms.RadioButton();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.buttonStop = new System.Windows.Forms.Button();
            this.label12 = new System.Windows.Forms.Label();
            this.label13 = new System.Windows.Forms.Label();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.label2 = new System.Windows.Forms.Label();
            this.label14 = new System.Windows.Forms.Label();
            this.label10 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.SuspendLayout();
            // 
            // pictureBox1
            // 
            this.pictureBox1.Location = new System.Drawing.Point(12, 12);
            this.pictureBox1.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(319, 251);
            this.pictureBox1.TabIndex = 0;
            this.pictureBox1.TabStop = false;
            // 
            // quitButton
            // 
            this.quitButton.Location = new System.Drawing.Point(619, 304);
            this.quitButton.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.quitButton.Name = "quitButton";
            this.quitButton.Size = new System.Drawing.Size(75, 23);
            this.quitButton.TabIndex = 1;
            this.quitButton.Text = "Quit";
            this.quitButton.UseVisualStyleBackColor = true;
            this.quitButton.Click += new System.EventHandler(this.quitButton_Click);
            // 
            // textBoxPhase1AvgTime
            // 
            this.textBoxPhase1AvgTime.Location = new System.Drawing.Point(67, 38);
            this.textBoxPhase1AvgTime.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxPhase1AvgTime.Name = "textBoxPhase1AvgTime";
            this.textBoxPhase1AvgTime.Size = new System.Drawing.Size(63, 22);
            this.textBoxPhase1AvgTime.TabIndex = 2;
            // 
            // textBoxPhase2AvgTime
            // 
            this.textBoxPhase2AvgTime.Location = new System.Drawing.Point(67, 66);
            this.textBoxPhase2AvgTime.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxPhase2AvgTime.Name = "textBoxPhase2AvgTime";
            this.textBoxPhase2AvgTime.Size = new System.Drawing.Size(63, 22);
            this.textBoxPhase2AvgTime.TabIndex = 2;
            // 
            // textBoxPhase3AvgTime
            // 
            this.textBoxPhase3AvgTime.Location = new System.Drawing.Point(67, 94);
            this.textBoxPhase3AvgTime.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxPhase3AvgTime.Name = "textBoxPhase3AvgTime";
            this.textBoxPhase3AvgTime.Size = new System.Drawing.Size(63, 22);
            this.textBoxPhase3AvgTime.TabIndex = 2;
            // 
            // textBoxPhase4AvgTime
            // 
            this.textBoxPhase4AvgTime.Location = new System.Drawing.Point(67, 122);
            this.textBoxPhase4AvgTime.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxPhase4AvgTime.Name = "textBoxPhase4AvgTime";
            this.textBoxPhase4AvgTime.Size = new System.Drawing.Size(63, 22);
            this.textBoxPhase4AvgTime.TabIndex = 2;
            // 
            // textBoxFileName
            // 
            this.textBoxFileName.Location = new System.Drawing.Point(12, 267);
            this.textBoxFileName.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxFileName.Name = "textBoxFileName";
            this.textBoxFileName.Size = new System.Drawing.Size(319, 22);
            this.textBoxFileName.TabIndex = 3;
            // 
            // textBoxImageCount
            // 
            this.textBoxImageCount.Location = new System.Drawing.Point(349, 267);
            this.textBoxImageCount.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxImageCount.Name = "textBoxImageCount";
            this.textBoxImageCount.Size = new System.Drawing.Size(63, 22);
            this.textBoxImageCount.TabIndex = 2;
            // 
            // textBoxFps
            // 
            this.textBoxFps.Location = new System.Drawing.Point(631, 267);
            this.textBoxFps.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxFps.Name = "textBoxFps";
            this.textBoxFps.Size = new System.Drawing.Size(63, 22);
            this.textBoxFps.TabIndex = 2;
            // 
            // textBoxQueue1AvgWait
            // 
            this.textBoxQueue1AvgWait.Location = new System.Drawing.Point(496, 64);
            this.textBoxQueue1AvgWait.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxQueue1AvgWait.Name = "textBoxQueue1AvgWait";
            this.textBoxQueue1AvgWait.Size = new System.Drawing.Size(63, 22);
            this.textBoxQueue1AvgWait.TabIndex = 2;
            this.textBoxQueue1AvgWait.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // textBoxQueue2AvgWait
            // 
            this.textBoxQueue2AvgWait.Location = new System.Drawing.Point(496, 92);
            this.textBoxQueue2AvgWait.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxQueue2AvgWait.Name = "textBoxQueue2AvgWait";
            this.textBoxQueue2AvgWait.Size = new System.Drawing.Size(63, 22);
            this.textBoxQueue2AvgWait.TabIndex = 2;
            this.textBoxQueue2AvgWait.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // textBoxQueue3AvgWait
            // 
            this.textBoxQueue3AvgWait.Location = new System.Drawing.Point(496, 121);
            this.textBoxQueue3AvgWait.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxQueue3AvgWait.Name = "textBoxQueue3AvgWait";
            this.textBoxQueue3AvgWait.Size = new System.Drawing.Size(63, 22);
            this.textBoxQueue3AvgWait.TabIndex = 2;
            this.textBoxQueue3AvgWait.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // textBoxQueueCount1
            // 
            this.textBoxQueueCount1.Location = new System.Drawing.Point(636, 63);
            this.textBoxQueueCount1.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxQueueCount1.Name = "textBoxQueueCount1";
            this.textBoxQueueCount1.Size = new System.Drawing.Size(63, 22);
            this.textBoxQueueCount1.TabIndex = 2;
            this.textBoxQueueCount1.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // textBoxQueueCount2
            // 
            this.textBoxQueueCount2.Location = new System.Drawing.Point(636, 91);
            this.textBoxQueueCount2.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxQueueCount2.Name = "textBoxQueueCount2";
            this.textBoxQueueCount2.Size = new System.Drawing.Size(63, 22);
            this.textBoxQueueCount2.TabIndex = 2;
            this.textBoxQueueCount2.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // textBoxQueueCount3
            // 
            this.textBoxQueueCount3.Location = new System.Drawing.Point(636, 119);
            this.textBoxQueueCount3.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.textBoxQueueCount3.Name = "textBoxQueueCount3";
            this.textBoxQueueCount3.Size = new System.Drawing.Size(63, 22);
            this.textBoxQueueCount3.TabIndex = 2;
            this.textBoxQueueCount3.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(20, 41);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(40, 17);
            this.label1.TabIndex = 4;
            this.label1.Text = "Load";
            this.label1.TextAlign = System.Drawing.ContentAlignment.TopRight;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(564, 95);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(63, 17);
            this.label3.TabIndex = 4;
            this.label3.Text = "Queue 2";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(564, 123);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(63, 17);
            this.label4.TabIndex = 4;
            this.label4.Text = "Queue 3";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(9, 69);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(51, 17);
            this.label5.TabIndex = 4;
            this.label5.Text = "Resize";
            this.label5.TextAlign = System.Drawing.ContentAlignment.TopRight;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(21, 97);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(39, 17);
            this.label6.TabIndex = 4;
            this.label6.Text = "Filter";
            this.label6.TextAlign = System.Drawing.ContentAlignment.TopRight;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(564, 68);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(63, 17);
            this.label7.TabIndex = 4;
            this.label7.Text = "Queue 1";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(9, 122);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(54, 17);
            this.label8.TabIndex = 4;
            this.label8.Text = "Display";
            this.label8.TextAlign = System.Drawing.ContentAlignment.TopRight;
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(8, 17);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(141, 17);
            this.label9.TabIndex = 4;
            this.label9.Text = "Time Per Phase (ms)";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(556, 246);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(139, 17);
            this.label11.TabIndex = 5;
            this.label11.Text = "Time Per Image (ms)";
            this.label11.TextAlign = System.Drawing.ContentAlignment.TopRight;
            // 
            // buttonStart
            // 
            this.buttonStart.Location = new System.Drawing.Point(432, 304);
            this.buttonStart.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.buttonStart.Name = "buttonStart";
            this.buttonStart.Size = new System.Drawing.Size(75, 23);
            this.buttonStart.TabIndex = 6;
            this.buttonStart.Text = "Start";
            this.buttonStart.UseVisualStyleBackColor = true;
            this.buttonStart.Click += new System.EventHandler(this.buttonStart_Click);
            // 
            // radioButtonSequential
            // 
            this.radioButtonSequential.AutoSize = true;
            this.radioButtonSequential.Location = new System.Drawing.Point(5, 17);
            this.radioButtonSequential.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.radioButtonSequential.Name = "radioButtonSequential";
            this.radioButtonSequential.Size = new System.Drawing.Size(96, 21);
            this.radioButtonSequential.TabIndex = 7;
            this.radioButtonSequential.Text = "Sequential";
            this.radioButtonSequential.UseVisualStyleBackColor = true;
            this.radioButtonSequential.CheckedChanged += new System.EventHandler(this.radioButtonSequential_CheckedChanged);
            // 
            // radioButtonPipeline
            // 
            this.radioButtonPipeline.AutoSize = true;
            this.radioButtonPipeline.Location = new System.Drawing.Point(108, 17);
            this.radioButtonPipeline.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.radioButtonPipeline.Name = "radioButtonPipeline";
            this.radioButtonPipeline.Size = new System.Drawing.Size(87, 21);
            this.radioButtonPipeline.TabIndex = 7;
            this.radioButtonPipeline.Text = "Pipelined";
            this.radioButtonPipeline.UseVisualStyleBackColor = true;
            this.radioButtonPipeline.CheckedChanged += new System.EventHandler(this.radioButtonPipeline_CheckedChanged);
            // 
            // radioButtonLoadBalanced
            // 
            this.radioButtonLoadBalanced.AutoSize = true;
            this.radioButtonLoadBalanced.Location = new System.Drawing.Point(201, 17);
            this.radioButtonLoadBalanced.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.radioButtonLoadBalanced.Name = "radioButtonLoadBalanced";
            this.radioButtonLoadBalanced.Size = new System.Drawing.Size(124, 21);
            this.radioButtonLoadBalanced.TabIndex = 7;
            this.radioButtonLoadBalanced.Text = "Load Balanced";
            this.radioButtonLoadBalanced.UseVisualStyleBackColor = true;
            this.radioButtonLoadBalanced.CheckedChanged += new System.EventHandler(this.radioButtonLoadBalanced_CheckedChanged);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.radioButtonLoadBalanced);
            this.groupBox1.Controls.Add(this.radioButtonPipeline);
            this.groupBox1.Controls.Add(this.radioButtonSequential);
            this.groupBox1.Location = new System.Drawing.Point(13, 287);
            this.groupBox1.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Padding = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.groupBox1.Size = new System.Drawing.Size(328, 47);
            this.groupBox1.TabIndex = 8;
            this.groupBox1.TabStop = false;
            // 
            // buttonStop
            // 
            this.buttonStop.Location = new System.Drawing.Point(521, 304);
            this.buttonStop.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.buttonStop.Name = "buttonStop";
            this.buttonStop.Size = new System.Drawing.Size(79, 23);
            this.buttonStop.TabIndex = 9;
            this.buttonStop.Text = "Stop";
            this.buttonStop.UseVisualStyleBackColor = true;
            this.buttonStop.Click += new System.EventHandler(this.buttonStop_Click);
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(644, 26);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(51, 17);
            this.label12.TabIndex = 10;
            this.label12.Text = "Queue";
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(660, 43);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(35, 17);
            this.label13.TabIndex = 10;
            this.label13.Text = "Size";
            this.label13.TextAlign = System.Drawing.ContentAlignment.TopRight;
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.label9);
            this.groupBox2.Controls.Add(this.label8);
            this.groupBox2.Controls.Add(this.label6);
            this.groupBox2.Controls.Add(this.label5);
            this.groupBox2.Controls.Add(this.label1);
            this.groupBox2.Controls.Add(this.textBoxPhase4AvgTime);
            this.groupBox2.Controls.Add(this.textBoxPhase3AvgTime);
            this.groupBox2.Controls.Add(this.textBoxPhase2AvgTime);
            this.groupBox2.Controls.Add(this.textBoxPhase1AvgTime);
            this.groupBox2.Location = new System.Drawing.Point(337, 14);
            this.groupBox2.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Padding = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.groupBox2.Size = new System.Drawing.Size(152, 160);
            this.groupBox2.TabIndex = 11;
            this.groupBox2.TabStop = false;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(508, 26);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(51, 17);
            this.label2.TabIndex = 10;
            this.label2.Text = "Queue";
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(492, 44);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(71, 17);
            this.label14.TabIndex = 10;
            this.label14.Text = "Wait Time";
            this.label14.TextAlign = System.Drawing.ContentAlignment.TopRight;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(352, 243);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(53, 17);
            this.label10.TabIndex = 12;
            this.label10.Text = "Images";
            // 
            // SlideshowForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(739, 353);
            this.Controls.Add(this.label10);
            this.Controls.Add(this.label11);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.label14);
            this.Controls.Add(this.label13);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label12);
            this.Controls.Add(this.buttonStop);
            this.Controls.Add(this.textBoxFps);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.buttonStart);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.textBoxFileName);
            this.Controls.Add(this.textBoxImageCount);
            this.Controls.Add(this.textBoxQueue3AvgWait);
            this.Controls.Add(this.textBoxQueue2AvgWait);
            this.Controls.Add(this.textBoxQueue1AvgWait);
            this.Controls.Add(this.textBoxQueueCount3);
            this.Controls.Add(this.textBoxQueueCount2);
            this.Controls.Add(this.textBoxQueueCount1);
            this.Controls.Add(this.quitButton);
            this.Controls.Add(this.pictureBox1);
            this.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.Name = "SlideshowForm";
            this.Text = "Image Pipeline";
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.Button quitButton;
        private System.Windows.Forms.TextBox textBoxPhase1AvgTime;
        private System.Windows.Forms.TextBox textBoxPhase2AvgTime;
        private System.Windows.Forms.TextBox textBoxPhase3AvgTime;
        private System.Windows.Forms.TextBox textBoxPhase4AvgTime;
        private System.Windows.Forms.TextBox textBoxFileName;
        private System.Windows.Forms.TextBox textBoxImageCount;
        private System.Windows.Forms.TextBox textBoxFps;
        private System.Windows.Forms.TextBox textBoxQueue1AvgWait;
        private System.Windows.Forms.TextBox textBoxQueue2AvgWait;
        private System.Windows.Forms.TextBox textBoxQueue3AvgWait;
        private System.Windows.Forms.TextBox textBoxQueueCount1;
        private System.Windows.Forms.TextBox textBoxQueueCount2;
        private System.Windows.Forms.TextBox textBoxQueueCount3;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.Button buttonStart;
        private System.Windows.Forms.RadioButton radioButtonSequential;
        private System.Windows.Forms.RadioButton radioButtonPipeline;
        private System.Windows.Forms.RadioButton radioButtonLoadBalanced;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button buttonStop;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.Label label10;
    }
}

