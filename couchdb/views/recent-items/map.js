function(doc) {
  if (doc.message) {
    emit(doc._id, doc);
  }
};