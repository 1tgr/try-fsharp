function(head, req) {  
    var row, snippets = [];

    start({
        headers: {
            "Content-Type": "text/html"
        }
    });

    while (row = getRow()) {
        snippets.push(row.value);
    }

    var Mustache = require("lib/mustache");
    var doc = {
        head: {
            title: "Snippets"
        },
        snippets: snippets
    };

    send(Mustache.to_html(this.templates.snippets, doc, this.templates.partials));
}
