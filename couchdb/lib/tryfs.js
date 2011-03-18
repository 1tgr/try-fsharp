function render(req, start, send, ddoc, template, docFn) {
    if (req.query.format === "json") {
        start({ headers: { "Content-Type": "application/json" } });
        var doc = docFn();
        send(JSON.stringify(doc));
    } else {
        start({ headers: { "Content-Type": "text/html" } });

        var doc = docFn();
        var name, partials = ddoc.templates.partials;

        for (name in partials) {
            if (partials.hasOwnProperty(name)) {
                if (doc[name] === undefined) {
                    doc[name] = {};
                }
            }
        }

        var Mustache = require("lib/mustache");
        send(Mustache.to_html(template, doc, ddoc.templates.partials));
    }
}

function renderSingle(req, ddoc, template, doc) {
    if (req.query.format === "json") {
        return JSON.stringify(doc);
    } else {
        var name, partials = ddoc.templates.partials;

        for (name in partials) {
            if (partials.hasOwnProperty(name)) {
                if (doc[name] === undefined) {
                    doc[name] = {};
                }
            }
        }

        var Mustache = require("lib/mustache");
        return Mustache.to_html(template, doc, ddoc.templates.partials);
    }
}

exports.render = render;
exports.renderSingle = renderSingle;
