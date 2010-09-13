//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Microsoft.Practices.ParallelGuideSamples.ImagePipeline
{
    // This sample does not use the MVP pattern to separate concerns of the view from
    // the model. This is largerly for reasons of simplicity in the sample code.
    // For further discussion of MVVM and MVP patterns see Appendix A. For an example
    // of MVVM is a WPF application see the Chapter 5 code.
    //
    public partial class MainForm : Form
    {
        enum ImageMode { Sequential, Pipelined, LoadBalanced }
        enum Mode { Stopped, Running, Stopping }

        delegate void UserCallback(object o);
        readonly UserCallback updateBitmapDelegate;
        readonly UserCallback cancelFinishedDelegate;
        readonly UserCallback showErrorDelegate;

        Task mainTask = null;
        CancellationTokenSource cts = null;
        ImageMode imageMode = ImageMode.Pipelined;
        Mode mode = Mode.Stopped;
        readonly Stopwatch sw = new Stopwatch();
        
        int imagesSoFar = 0;
        readonly int[] totalTime = {0, 0, 0, 0, 0, 0, 0, 0};

        public MainForm()
        {
            InitializeComponent();
            cancelFinishedDelegate = new UserCallback(o =>
            {
                this.cts = null;
                this.mainTask = null;
                this.mode = Mode.Stopped;
                UpdateEnabledStatus();
            });

            updateBitmapDelegate = new UserCallback(SetBitmap);

            showErrorDelegate = new UserCallback(obj =>
            {
                Exception e = (Exception)obj;
                MessageBox.Show(e.Message, "Application Error", MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
                this.cts = null;
                this.mainTask = null;
                this.mode = Mode.Stopped;
                UpdateEnabledStatus();
            });

            UpdateEnabledStatus();            
        }

        private void UpdateEnabledStatus()
        {
            radioButtonSequential.Enabled = (mode == Mode.Stopped);
            radioButtonPipeline.Enabled = (mode == Mode.Stopped);
            radioButtonLoadBalanced.Enabled = (mode == Mode.Stopped);
            buttonStart.Enabled = (mode == Mode.Stopped);
            buttonStop.Enabled = (mode == Mode.Running);
            quitButton.Enabled = (mode == Mode.Stopped);
            radioButtonSequential.Checked = (imageMode == ImageMode.Sequential);
            radioButtonPipeline.Checked = (imageMode == ImageMode.Pipelined);
            radioButtonLoadBalanced.Checked = (imageMode == ImageMode.LoadBalanced);
        }

        private void SetBitmap(object info)
        {
            var priorImage = this.pictureBox1.Image;
            var imageInfo = (ImageInfo)info;
            this.pictureBox1.Image = imageInfo.FilteredImage;
            imageInfo.FilteredImage = null;
            this.imagesSoFar += 1;

            // calculate duration of each phase
            for (int i = 0; i < 4; i++)
            {
                this.totalTime[i] += imageInfo.PhaseEndTick[i] - imageInfo.PhaseStartTick[i];
            }
            // infer queue wait times by comparing phase(n+1) start with phase(n) finish timestamp
            this.totalTime[4] += imageInfo.PhaseStartTick[1] - imageInfo.PhaseEndTick[0];
            this.totalTime[5] += imageInfo.PhaseStartTick[2] - imageInfo.PhaseEndTick[1];
            this.totalTime[6] += imageInfo.PhaseStartTick[3] - imageInfo.PhaseEndTick[2];

            this.textBoxPhase1AvgTime.Text = (this.totalTime[0] / this.imagesSoFar).ToString();
            this.textBoxPhase2AvgTime.Text = (this.totalTime[1] / this.imagesSoFar).ToString();
            this.textBoxPhase3AvgTime.Text = (this.totalTime[2] / this.imagesSoFar).ToString();
            this.textBoxPhase4AvgTime.Text = (this.totalTime[3] / this.imagesSoFar).ToString();

            this.textBoxQueue1AvgWait.Text = (this.totalTime[4] / this.imagesSoFar).ToString();
            this.textBoxQueue2AvgWait.Text = (this.totalTime[5] / this.imagesSoFar).ToString();
            this.textBoxQueue3AvgWait.Text = (this.totalTime[6] / this.imagesSoFar).ToString();

            this.textBoxQueueCount1.Text = imageInfo.QueueCount1.ToString();
            this.textBoxQueueCount2.Text = imageInfo.QueueCount2.ToString();
            this.textBoxQueueCount3.Text = imageInfo.QueueCount3.ToString();

            this.textBoxFileName.Text = imageInfo.FileName;
            this.textBoxImageCount.Text = imageInfo.ImageCount.ToString();

            long elapsedTime = this.sw.ElapsedMilliseconds;
            this.textBoxFps.Text =
                string.Format("{0: 0}", elapsedTime / imageInfo.ImageCount);                 

            if (priorImage != null) priorImage.Dispose();

            if (imageInfo.SequenceNumber != imagesSoFar - 1)
            {
                var msg = string.Format("Program error-- images are out of order. Expected {0} but received {1}",
                     imagesSoFar - 1, imageInfo.SequenceNumber);
                MessageBox.Show(msg, "Application Error", MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
                Application.Exit();
            }
        }

        private void quitButton_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }

        private void buttonStart_Click(object sender, EventArgs e)
        {
            if (mainTask == null)
            {
                mode = Mode.Running;
                UpdateEnabledStatus();                
                Action<ImageInfo> updateFn = (ImageInfo bm) => this.Invoke(this.updateBitmapDelegate, (object)bm);
                Action<Exception> errorFn = (Exception ex) => this.Invoke(this.showErrorDelegate, (object)ex);
                cts = new CancellationTokenSource();
                int enumVal = (int)imageMode;
                this.sw.Restart();
                imagesSoFar = 0;
                for (int i = 0; i < totalTime.Length; i++) totalTime[i] = 0;
                mainTask = Task.Factory.StartNew(() => ImagePipeline.ImagePipelineMainLoop(updateFn, cts.Token, enumVal, errorFn),
                    cts.Token,
                    TaskCreationOptions.LongRunning,
                    TaskScheduler.Default);                
            }
        }

        private void radioButtonSequential_CheckedChanged(object sender, EventArgs e)
        {
            if (radioButtonSequential.Checked)
                imageMode = ImageMode.Sequential;
        }

        private void radioButtonPipeline_CheckedChanged(object sender, EventArgs e)
        {
            if (radioButtonPipeline.Checked)
                imageMode = ImageMode.Pipelined;
        }

        private void radioButtonLoadBalanced_CheckedChanged(object sender, EventArgs e)
        {
            if (radioButtonLoadBalanced.Checked)
                imageMode = ImageMode.LoadBalanced;
        }

        private void buttonStop_Click(object sender, EventArgs e)
        {
            if (cts != null)
            {
                mode = Mode.Stopping;
                UpdateEnabledStatus();
                cts.Cancel();

                Task.Factory.StartNew(() =>
                    {
                        mainTask.Wait();
                        this.Invoke(cancelFinishedDelegate, this);
                    });
            }
        }
    }
}
