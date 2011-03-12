function(head, req) {
    var isJson = req.query.format === "json";
    var userId = req.query.userId;
    var isFSSnip = userId === "fssnip";
    var row, snippets = [];

    start({ headers: { "Content-Type": isJson ? "application/json" : "text/html" } });

    while (row = getRow()) {
        if (row.value.userId === userId || (isFSSnip && !row.value.private)) {
            snippets.push(row.value);
        }
    }

    var Mustache = require("lib/mustache");
    var doc = {
        head: {
            title: isFSSnip ? "Snippets" : "Snippets for " + userId
        },
        isFSSnip: isFSSnip,
        userId: userId,
        snippets: snippets
    };

    if (isJson) {
        send(JSON.stringify(doc));
    } else {
        send(Mustache.to_html(this.templates.snippets, doc, this.templates.partials));
    }
}
