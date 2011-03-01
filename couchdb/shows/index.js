function() {  
    var Mustache = require("lib/mustache");
    var doc = {
        head: { title: "Try F#" }
    };

    return Mustache.to_html(this.templates.index, doc, this.templates.partials);
}
