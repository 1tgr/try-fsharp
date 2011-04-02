function(doc) {
    if (doc.type === "type") {
        emit(doc.namespace + "." + doc.name, {
            _rev: doc._rev,
            assemblyName: doc.assemblyName
        });
    }
}
