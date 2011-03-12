function(head, req) {
    var userId = req.query.userId || null;
    var matches =
        userId
        ? function(row) { return row.value.userId === userId; }
        : function(row) { return !row.value.private; };

    var row, snippets = [];

    start({
        headers: {
            "Content-Type": "text/html"
        }
    });

    while (row = getRow()) {
        if (matches(row)) {
            snippets.push(row.value);
        }
    }

    var Mustache = require("lib/mustache");
    var doc = {
        head: {
            title: userId ? "Snippets for " + userId : "Snippets"
        },
        userId: userId,
        snippets: snippets
    };

    if (req.query.format === "json") {
        send(JSON.stringify(doc));
    } else {
        send(Mustache.to_html(this.templates.snippets, doc, this.templates.partials));
    }
}
