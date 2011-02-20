function init() {
    var sessionId;
    var db = $.couch.db("tryfs");

    function onChange(resp) {
      $.each(resp.results, function() {
        var message = this.doc.message ? this.doc.message : "\n";
        console.commandResult(message, "jquery-console-message-success");
      });
    }

    function subscribe() {
        if (!sessionId) {
            sessionId  = $.couch.newUUID();
            db.changes(null, { filter: "app/session", include_docs: true, sessionId: sessionId }).onChange(onChange);
        }
    }

    function commandHandle(code) {
        subscribe();
        db.saveDoc({ messageType: "in", message: code, sessionId: sessionId });
        return true;
    }

    var console = $("#console").console({
        welcomeMessage: "",
        commandHandle: commandHandle
    });

    function onSend() {
        subscribe();
        var code = $("#code").val() + ";;";
        console.commandResult(code, "jquery-console-message-success");
        db.saveDoc({ messageType: "in", message: code, sessionId: sessionId });
    }

    $("#send").click(onSend);
}

$(document).ready(init);

