function(doc) {
  emit(doc.sessionId, { messageType: doc.messageType, message: doc.message });
}