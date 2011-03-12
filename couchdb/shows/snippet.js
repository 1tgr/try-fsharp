function(doc, req) {  
    var isJson = req.query.format === "json";
    doc.head = { title: doc.title };

    if (isJson) {
        send(JSON.stringify(doc));
    } else {
        var Mustache = require("lib/mustache");
        send(Mustache.to_html(this.templates.snippet, doc, this.templates.partials));
    }
}
