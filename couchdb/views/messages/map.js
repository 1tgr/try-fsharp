function(doc) {
    if (doc.messageType) {
        emit(doc.date, doc);
    }
}
