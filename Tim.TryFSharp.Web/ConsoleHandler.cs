using System;
using System.Text;
using System.Web;
using System.Web.SessionState;

namespace Tim.TryFSharp.Web
{
    public class ConsoleHandler : IHttpHandler, IReadOnlySessionState
    {
        public bool IsReusable
        {
            get { return true; }
        }

        public void ProcessRequest(HttpContext context)
        {
            lock (context.Session.SyncRoot)
            {
                StringBuilder buffer = (StringBuilder)context.Session["buffer"];
                context.Response.ContentType = "text/plain";
                if (buffer != null)
                    context.Response.Write(buffer);
            }
        }
    }
}
