using System;
using System.Collections.Generic;
using System.Text;

namespace Tim.TryFSharp.Web
{
    public class ConsoleBuffer
    {
        private readonly object syncRoot = new object();
        private readonly List<string> messages = new List<string>();

        public void AppendLine(string text)
        {
            lock (syncRoot)
                messages.Add(text);
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