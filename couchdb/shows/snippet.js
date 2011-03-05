function(doc, req) {  
    var Mustache = require("lib/mustache");
    doc.head = { title: doc.title };
    return Mustache.to_html(this.templates.snippet, doc, this.templates.partials);
}
