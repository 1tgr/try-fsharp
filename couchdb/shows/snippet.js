function(doc, req) {  
    var TryFS = require("lib/tryfs");
    return TryFS.renderSingle(req, this, this.templates.snippet, doc);
}
