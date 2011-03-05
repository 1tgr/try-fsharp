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

    function subscribe(success) {
        if (sessionId) {
            success(sessionId);
        } else {
            var session = {
                initNames: [ "init" ],
                initTexts: [ "printfn \"hello\"" ]
            };
            
            db.saveDoc(
                session,
                {
                    success: function(resp) {
                        sessionId = resp.id;
                        $.get(
                            "/tryfs/",
                            { },
                            function(info) {
                                db.changes(info.update_seq, { filter: "app/session", include_docs: true, sessionId: sessionId }).onChange(onChange);
                            },
                            "json");

                        success(sessionId);
                    }
                });
        }
    }

    function commandHandle(code) {
        subscribe(function(sessionId) {
            db.saveDoc({ messageType: "in", message: code, sessionId: sessionId });
        });
        
        return true;
    }

    var console = $("#console").console({
        welcomeMessage: "",
        commandHandle: commandHandle
    });

    function onSend() {
        var code = $("#code").val() + ";;";
        subscribe(function(sessionId) {
            db.saveDoc({ messageType: "in", message: code, sessionId: sessionId });
            console.commandResult(code, "jquery-console-message-success");
        });
    }

    $("#send").click(onSend);
}

$(document).ready(init);

