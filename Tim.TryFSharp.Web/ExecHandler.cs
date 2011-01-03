using System;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Web;
using System.Web.SessionState;
using Microsoft.Win32.SafeHandles;

namespace Tim.TryFSharp.Web
{
    public class ExecHandler : IHttpHandler, IRequiresSessionState
    {
        [DllImport("kernel32.dll")]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool AssignProcessToJobObject(IntPtr hJob, IntPtr hProcess);

        public bool IsReusable
        {
            get { return true; }
        }

        public void ProcessRequest(HttpContext context)
        {
            lock (context.Session.SyncRoot)
            {
                Process process = (Process) context.Session["fsi"];
                if (process != null && process.HasExited)
                {
                    process.Dispose();
                    process = null;
                }

                if (process == null)
                {
                    string programFiles = Environment.GetEnvironmentVariable("ProgramFiles(x86)");
                    if (string.IsNullOrEmpty(programFiles))
                        programFiles = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);

                    ProcessStartInfo startInfo = new ProcessStartInfo();
                    startInfo.FileName = Path.Combine(programFiles, @"Microsoft F#\v4.0\fsi.exe");
                    startInfo.WorkingDirectory = Path.GetTempPath();
                    startInfo.Arguments = string.Format(@"-I ""{0}\bin"" -r Tim.TryFSharp.Interactive.dll", AppDomain.CurrentDomain.BaseDirectory);
                    startInfo.RedirectStandardError = true;
                    startInfo.RedirectStandardInput = true;
                    startInfo.RedirectStandardOutput = true;
                    startInfo.UseShellExecute = false;

                    process = new Process();
                    process.StartInfo = startInfo;
                    process.Start();

                    SafeFileHandle handle = (SafeFileHandle) context.Application["job"];
                    AssignProcessToJobObject(handle.DangerousGetHandle(), process.Handle);

                    ConsoleBuffer buffer = new ConsoleBuffer();
                    DataReceivedEventHandler handler = (_, e) => { buffer.AppendLine(e.Data);  };
                    process.ErrorDataReceived += handler;
                    process.OutputDataReceived += handler;
                    process.BeginErrorReadLine();
                    process.BeginOutputReadLine();
                    context.Session.Add("fsi", process);
                    context.Session.Add("buffer", buffer);
                }

                process.StandardInput.WriteLine(context.Request.Form["code"]);
            }
        }
    }
}
