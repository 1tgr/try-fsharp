using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Web;
using System.Web.Security;
using Microsoft.Win32.SafeHandles;

namespace Tim.TryFSharp.Web
{
    public class Global : HttpApplication
    {
        [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
        private static extern IntPtr CreateJobObject(IntPtr lpJobAttributes, string lpName);

        [DllImport("kernel32.dll")]
        private static extern bool TerminateJobObject(IntPtr hJob, uint uExitCode);

        protected void Application_Start(object sender, EventArgs e)
        {
            SafeFileHandle handle = new SafeFileHandle(CreateJobObject(IntPtr.Zero, null), true);
            Application.Add("job", handle);
        }

        protected void Application_End(object sender, EventArgs e)
        {
            SafeFileHandle handle = (SafeFileHandle) Application["job"];
            if (handle != null)
            {
                Application.Remove("job");
                TerminateJobObject(handle.DangerousGetHandle(), 0);
                handle.Close();
            }
        }

        protected void Session_Start(object sender, EventArgs e)
        {
            if(Response.Cookies.Count > 0)
               foreach(string s in Response.Cookies.AllKeys)
                  if(s == FormsAuthentication.FormsCookieName || s.ToLower().Equals("asp.net_sessionid") )
                     Response.Cookies[s].HttpOnly = false;
        }

        protected void Session_End(object sender, EventArgs e)
        {
            lock (Session.SyncRoot)
            {
                Process process = (Process) Session["fsi"];
                if (process != null)
                {
                    Session.Remove("fsi");
                    process.StandardInput.WriteLine("#quit;;");
                    process.Dispose();
                }
            }
        }
    }
}
