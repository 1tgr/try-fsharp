function(doc) {
    if (doc.type == "session") {
        emit([ doc.host, doc.fsiPid ], null);
    }  
}
