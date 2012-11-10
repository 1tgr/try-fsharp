function(doc) {
    if (doc.type == "session") {
        emit(doc.started, { _id: doc._id, _rev: doc._rev, host: doc.host, fsiPid: doc.fsiPid, servicePid: doc.servicePid });
    }  
}
