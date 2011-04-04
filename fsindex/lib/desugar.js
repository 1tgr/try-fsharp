function desugarArray(a) {
    var i, ret = new Array(a.length);

    for (i = 0; i < ret.length; i++) {
        ret[i] = desugar(a[i]);
    }

    return ret;
};

function desugar(t) {
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

if (typeof exports === "object") {
    exports.desugar = desugar;
    exports.desugarArray = desugarArray;
}
