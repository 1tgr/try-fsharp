// Work around IE8 bug in XMLHttpRequest
// http://bugs.jquery.com/ticket/6437
$(function () {
  $.ajaxSetup({
    xhr: function() {
      if ($.browser.msie) {
        return new ActiveXObject("Microsoft.XMLHTTP");
      } else {
        return new XMLHttpRequest();
      }
    }
  })
});

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
            sessionId = $.couch.newUUID();
            $.get(
                "/tryfs/",
                { },
                function(info) {
                    db.changes(info.update_seq, { filter: "app/session", include_docs: true, sessionId: sessionId }).onChange(onChange);
                },
                "json");
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

