function words(s) {
    var i, word = "", ret = [];
    var isSymbol, symbols = ":()<>-*";

    for (i = 0; i < s.length; i++) {
        isSymbol = symbols.indexOf(s[i]) > -1;
        if (s[i] === " " || isSymbol) {
            if (word.length > 0) {
                ret.push(word);
            }

            if (isSymbol) {
                if (ret.length > 0 && ret[ret.length - 1] === "-" && s[i] === ">") {
                    ret[ret.length - 1] = "->";
                } else {
                    ret.push(s[i]);
                }
            }

            word = "";
        } else {
            word += s[i];
        }
    }

    if (word.length > 0) {
        ret.push(word);
    }

    return ret;
}

function parse(start, w) {
    var i, state = 0;
    var info = { }, info2, tuple, arg;
    var error = null;

    for (i = start; i < w.length && state !== 99; i++) {
        if (error) {
            throw error;
        }

        switch (state) {
        case 0: // expecting name or ':'
            if (w[i] === ":") {
                info.prototype = [];
                state = 1;
                break;
            } else if (w[i] === "(") {
                info.prototype = [];
                state = 1;
                // fall through
            } else {
                info.name = w[i];
                state = 2;
                break;
            }

        case 1: // expecting type
            if (w[i] === "->") {
                error = "expecting type";
            } else if (w[i] === "(") {
                info2 = parse(i + 1, w);
                i = info2.index;
                info.prototype.push(info2.prototype || [info2.name]);
            } else {
                info.prototype.push(w[i]);
                state = 3;
            }
            break;

        case 2: // expecting ':' , '->' or post-fix generic type
            if (w[i] === ":") {
                info.prototype = [];
                state = 1;
                break;
            } else {
                info.prototype = [ info.name ];
                delete info.name;
                // fall through
            }

        case 3: // expecting '->' or post-fix generic type
            if (w[i] === "->") {
                state = 1;
            } else if (w[i] === ")") {
                state = 99;
            } else if (w[i] === "*") {
                state = 4;

                tuple = info.prototype.pop();
                if (tuple.tuple) {
                    info.prototype.push(tuple);
                } else {
                    info.prototype.push({ tuple: [ tuple ] });
                }
            } else {
                state = 3;

                arg = info.prototype.pop();
                if (w[i] === "array") {
                    info.prototype.push({ array: arg });
                } else {
                    info.prototype.push({ tyCon: w[i], args: [ arg ] });
                }
            }
            break;

        case 4: // expecting tuple type
            if (w[i] === "->") {
                state = 1;
            } else if (w[i] === ")") {
                state = 99;
            } else if (w[i] === "*") {
                error = "expecting type";
            } else {
                tuple = info.prototype.pop();
                tuple.tuple.push(w[i]);
                info.prototype.push(tuple);
                state = 3;
            }
            break;
        }
    }
   
    info.index = i;
    return info;
}

function makeQuery(info) {
    if (info.name && info.prototype) {
        throw "can't search by name and prototype yet";
    } else if (info.name) {
        return {
            view: "app/by-name",
            format: function(rows) {
                return { byName: rows };
            },
            options: {
                startkey: info.name,
                endkey: info.name + "\ufff0"
            }
        };
    } else if (info.prototype) {
        var format = function(reverse) {
            return function(rows) {
                var i;

                for (i = 0; i < rows.length; i++) {
                    if (reverse) {
                        rows[i].key.reverse();
                    }

                    rows[i].signature = formatSignature(rows[i].key);
                }

                return { bySignature: rows };
            }
        };

        var a = [ ].concat(info.prototype);
        if (a.length > 0) {
            var first = a.shift();
            if (first === "_") {
                a.reverse();
                return {
                    view: "app/by-signature-reverse",
                    format: format(true),
                    options: {
                        startkey: a.concat(null),
                        endkey: a.concat({ "\ufff0": "" })
                    }
                };
            } else {
                a.unshift(first);
                var last = a.pop();
                if (last === "_") {
                    return {
                        view: "app/by-signature",
                        format: format(false),
                        options: {
                            startkey: a.concat(null),
                            endkey: a.concat({ "\ufff0": "" })
                        }
                    };
                } else if (typeof last === "string") {
                    return {
                        view: "app/by-signature",
                        format: format(false),
                        options: {
                            startkey: a.concat(last),
                            endkey: a.concat(last + "\ufff0")
                        }
                    };
                } else {
                    return {
                        view: "app/by-signature",
                        format: format(false),
                        options: {
                            key: a.concat(last)
                        }
                    };
                }
            }
        } else {
            return {
                view: "app/by-signature",
                format: format(false),
                options: {
                    startkey: [ null ],
                    endkey: [ { "\ufff0": "" } ]
                }
            };
        }
    } else {
        return null;
    }
}

function formatTypeName(t) {
    var s, i;

    if (typeof t === "string") {
        return t;
    } else if (t instanceof Array) {
        return "(" + formatSignature(t) + ")";
    } else if (t.array) {
        return formatTypeName(t.array) + " array";
    } else if (t.tyCon) {
        if (t.args.length === 1 && t.tyCon in { "array": 1, "list": 1, "option": 1 }) {
            return formatTypeName(t.args[0]) + " " + formatTypeName(t.tyCon);
        } else {
            s = new Array(t.args.length);

            for (i = 0; i < s.length; i++) {
                s[i] = formatTypeName(t.args[i]);
            }

            return formatTypeName(t.tyCon) + "<" + s.join(", ") + ">";
        }
    } else if (t.tuple) {
        s = new Array(t.tuple.length);

        for (i = 0; i < s.length; i++) {
            s[i] = formatTypeName(t.tuple[i]);
        }

        return s.join(" * ");
    } else {
        return JSON.stringify(t);
    }
}

function formatSignature(prototype) {
    var i, a = new Array(prototype.length);

    for (i = 0; i < a.length; i++) {
        a[i] = formatTypeName(prototype[i]);
    }

    return a.join(" -> ");
}

$(function() {
    var db = $.couch.db("fsindex");

    db.openDoc(
        "_design/app",
        {
            success: function(ddoc) {
                $("#search").livesearch({
                    minimumSearchLength: 1,
                    searchCallback: function(term) {
                        var w = words(term);
                        var info = parse(0, w);
                        var query = makeQuery(info);

                        var showResults = function(rows) {
                            var data = {
                                rows: rows,
                                name: info.name 
                            };

                            if (info.prototype) {
                                data.prototype = formatSignature(info.prototype);
                            }

                            var s = Mustache.to_html(ddoc.templates["search-results"], data);
                            $("#results").html(s);
                        };

                        if (query) {
                            query.options.limit = 100;
                            query.options.success = function(results) {
                                var rows = results.rows.length === 0 ? [] : query.format(results.rows);
                                showResults(rows);
                            };
                               
                            db.view(query.view, query.options);
                        } else {
                            showResults([ ]);
                        }
                    }
                });
            }
       });
});
