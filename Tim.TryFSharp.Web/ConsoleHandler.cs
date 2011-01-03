using System;
using System.Collections.Generic;
using System.Text;
using System.Web;
using System.Web.SessionState;
using Newtonsoft.Json;

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
            int since;
            string text;
            ConsoleBuffer buffer = (ConsoleBuffer)context.Session["buffer"];
            if (buffer == null)
            {
                since = 1;
                text = "";
            }
            else
            {
                string s = context.Request.QueryString["since"];
                if (s == null || !int.TryParse(s, out since))
                    since = 1;

                since = buffer.ReadSince(since, out text);
            }

            context.Response.ContentType = "application/json";

            using (JsonWriter writer = new JsonTextWriter(context.Response.Output))
            {
                JsonSerializer serializer = new JsonSerializer();

                Dictionary<string, object> dict = new Dictionary<string, object>
                    {
                        { "since", since },
                        { "text", text }
                    };

                serializer.Serialize(writer, dict);
            }
        }
    }
}
