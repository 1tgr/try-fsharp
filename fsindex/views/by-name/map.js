function(doc) {
    var i, typeKey;

    if (doc.type === "type") {
        typeKey = { _rev: doc._rev, namespace: doc.namespace, name: doc.name };
        emit([doc.name, "type"], typeKey);

        for (i in doc.methods) {
            emit([doc.methods[i].name, "method"], typeKey);
        }
        
        for (i in doc.properties) {
            emit([doc.properties[i].name, "property"], typeKey);
        }
    
        for (i in doc.events) {
            emit([doc.events[i].name, "event"], typeKey);
        }

        for (i in doc.fields) {
            emit([doc.fields[i].name, "field"], typeKey);
        }
    }
}
