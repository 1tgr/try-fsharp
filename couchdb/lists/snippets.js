function(head, req) {
    var TryFS = require("lib/tryfs");
    TryFS.render(req, start, send, this, this.templates.snippets, function() {
        var userId = req.query.userId;
        var isFSSnip = userId === "fssnip";
        var row, snippets = [];

        while (row = getRow()) {
            if (row.value.userId === userId || (isFSSnip && !row.value.private)) {
                snippets.push(row.value);
            }
        }

        return {
            title: isFSSnip ? "Snippets" : "Snippets for " + userId,
            isFSSnip: isFSSnip,
            userId: userId,
            snippets: snippets
        };
    });
}
