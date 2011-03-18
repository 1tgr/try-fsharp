function(head, req) {  
    var TryFS = require("lib/tryfs");
    TryFS.render(req, start, send, this, this.templates.index, function() {
        var row, snippets = [];

        while (row = getRow()) {
            if (row.value.userId === "fssnip") {
                snippets.push(row.value);
            }
        }

        return {
            indexTitle: true,
            title: "Try F#",
            snippets: snippets
        };
    });
}
