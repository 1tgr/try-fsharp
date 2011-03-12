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
            title: "Try F#"
        },
        snippets: snippets
    };

    if (req.query.format === "json") {
        send(JSON.stringify(doc));
    } else {
        send(Mustache.to_html(this.templates.index, doc, this.templates.partials));
    }
}
