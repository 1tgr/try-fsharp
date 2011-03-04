function(doc, req) {  
    var Mustache = require("lib/mustache");
    doc.head = { title: doc.title };
    doc.original = { link: doc.link };

    var partials = this.templates.partials;
    partials.original =
        doc.link
        ? partials.snippet_original
        : partials.snippet_nooriginal;

    return Mustache.to_html(this.templates.snippet, doc, partials);
}
