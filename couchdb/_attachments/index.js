function init() {
    var sessionId = $.couch.newUUID();
    var db = $.couch.db("tryfs");

    function commandHandle(code) {
        db.saveDoc({ messageType: "in", message: code, sessionId: sessionId });
        return true;
    }

    var console = $("#console").console({
        welcomeMessage: "",
        commandHandle: commandHandle
    });

    function onSend() {
        var code = $("#code").val() + ";;";
        console.commandResult(code, "jquery-console-message-success");
        db.saveDoc({ messageType: "in", message: code, sessionId: sessionId });
    }

    function onChange(resp) {
      $.each(resp.results, function() {
        if (this.doc.messageType == "out") {
          console.commandResult(this.doc.message, "jquery-console-message-success");
        }
      });
    }

    function subscribe() {
      db.changes(null, { filter: "app/session", include_docs: true, sessionId: sessionId }).onChange(onChange);
    } 

    setTimeout(subscribe, 500);
    $("#send").click(onSend);
}

$(document).ready(init);

