using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using Newtonsoft.Json;

namespace Tim.TryFSharp.Web
{
    public class ConsoleBuffer
    {
        private readonly string sessionId;
        private readonly object syncRoot = new object();
        private readonly List<string> messages = new List<string>();

        public ConsoleBuffer(string sessionId)
        {
            this.sessionId = sessionId;
        }

        public static void Log(string sessionId, string type, string text)
        {
            try
            {
                WebRequest request = WebRequest.Create("http://www.partario.com/couchdb/tryfs");
                request.ContentType = "application/json";
                request.Method = "POST";

                using (TextWriter textWriter = new StreamWriter(request.GetRequestStream()))
                using (JsonWriter writer = new JsonTextWriter(textWriter))
                {
                    JsonSerializer serializer = new JsonSerializer();

                    Dictionary<string, object> dict = new Dictionary<string, object>()
                        {
                            { "date", DateTime.Now.ToString("O") },
                            { "messageType", type },
                            { "sessionId", sessionId },
                            { "message", text },
                        };

                    serializer.Serialize(writer, dict);
                }

                using (WebResponse response = request.GetResponse())
                using (TextReader textReader = new StreamReader(response.GetResponseStream()))
                {
                    string s = textReader.ReadToEnd();
                }
            }
            catch { }
        }

        public void AppendLine(string text)
        {
            lock (syncRoot)
                messages.Add(text);

            Log(sessionId, "out", text);
        }

        public int ReadSince(int since, out string text)
        {
            lock (syncRoot)
            {
                StringBuilder sb = new StringBuilder();
                for (int i = since; i < messages.Count; i++)
                    sb.AppendLine(messages[i]);

                text = sb.ToString();
                return messages.Count;
            }
        }
    }
}
