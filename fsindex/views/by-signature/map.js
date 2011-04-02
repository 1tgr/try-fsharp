function(doc) {
    var desugar, desugarArray, member, i;

    desugarArray = function(a) {
        var i, ret = new Array(a.length);

        for (i = 0; i < ret.length; i++) {
            ret[i] = desugar(a[i]);
        }

        return ret;
    };

    desugar = function(t) {
        if (t.array) {
            return { array: desugar(t.array.element) };
        } else if (t.byRef) {
            return { byRef: desugar(t.byRef.element) };
        } else if (t.func) {
            return desugarArray(t.func.prototype);
        } else if (t.generic) {
            return t.generic.name;
        } else if (t.genericApp) {
            return {
                tyCon: desugar(t.genericApp.tyCon),
                args: desugarArray(t.genericApp.args)
            };
        } else if (t.keyword) {
            return t.keyword.name;
        } else if (t.tuple) {
            return { tuple: desugarArray(t.tuple.args) };
        } else if (t.def) {
            return t.def.name;
        } else {
            return t;
        }
    };

    member = function(m) {
        if (m.prototype) {
            emit(desugarArray(m.prototype), { name: m.name, type: typeKey });
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
