function(doc) {
    var i, typeKey;

    if (doc.type === "type") {
        typeKey = { _rev: doc._rev, namespace: doc.namespace, name: doc.name };
        emit(doc.name, typeKey);

        for (i in doc.methods) {
            emit(doc.methods[i].name, typeKey);
        }
        
        for (i in doc.properties) {
            emit(doc.properties[i].name, typeKey);
        }
    
        for (i in doc.events) {
            emit(doc.events[i].name, typeKey);
        }

        for (i in doc.fields) {
            emit(doc.fields[i].name, typeKey);
        }
    }
}
