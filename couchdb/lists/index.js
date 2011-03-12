function(head, req) {  
    var isJson = req.query.format === "json";
    var row, snippets = [];

    start({ headers: { "Content-Type": isJson ? "application/json" : "text/html" } });

    while (row = getRow()) {
        if (row.value.userId === "fssnip") {
            snippets.push(row.value);
        }
    }

    var doc = {
        head: {
            title: "Try F#"
        },
        snippets: snippets
    };

    if (isJson) {
        send(JSON.stringify(doc));
    } else {
        var Mustache = require("lib/mustache");
        send(Mustache.to_html(this.templates.index, doc, this.templates.partials));
    }
}
