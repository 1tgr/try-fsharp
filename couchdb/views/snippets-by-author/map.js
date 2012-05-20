function(doc) {
  if (doc.type === "snippet") {
    var doc2 = {};
    for (name in doc) {
      if (doc.hasOwnProperty(name)) {
        doc2[name] = doc[name];
      }
    }

    delete doc2.code;
    emit([ doc2.author, doc2.title ], doc2);
  }
}
