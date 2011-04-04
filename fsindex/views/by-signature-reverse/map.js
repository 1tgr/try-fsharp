function(doc) {
    // !code lib/desugar.js

    member = function(m) {
        if (m.prototype) {
            var a = desugarArray(m.prototype);
            a.reverse();
            emit(a, { name: m.name, type: typeKey });
        }
    };

    if (doc.type === "type") {
        typeKey = { _rev: doc._rev, namespace: doc.namespace, name: doc.name };

        for (i in doc.methods) {
            member(doc.methods[i]);
        }

        for (i in doc.properties) {
            member(doc.properties[i]);
        }
    }
}
