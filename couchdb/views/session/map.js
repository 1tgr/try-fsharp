function(doc) {
  if (typeof doc.sessionId !== "undefined" && typeof doc.messageType !== "undefined") {
    emit(doc.sessionId, { messageType: doc.messageType, message: doc.message });
  }
}
