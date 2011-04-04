function(doc) {
  if (doc.type === "type") {
    emit(doc.assembly, { _rev: doc._rev, namespace: doc.namespace, name: doc.name });
  }
}
