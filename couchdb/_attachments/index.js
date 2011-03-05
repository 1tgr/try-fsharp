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
    var subscription;
    var db = $.couch.db("tryfs");

    function onChange(resp) {
      $.each(resp.results, function() {
        var message = this.doc.message ? this.doc.message : "\n";
        console.commandResult(message, "jquery-console-message-success");
      });
    }

    function subscribe(success) {
        if (subscription) {
            success(subscription.session);
        } else {
            subscription = {
                session: {
                    type: "session",
                    initNames: [ "init" ],
                    initTexts: [ $("#code").val() ]
                }
            };
            
            $.get(
                "/tryfs/",
                { },
                function(info) {
                    db.saveDoc(
                        subscription.session,
                        {
                            success: function(resp) {
                                subscription.session = resp;

                                var options ={
                                    filter: "app/session",
                                    include_docs: true,
                                    sessionId: subscription.session.id
                                };

                                subscription.promise = db.changes(info.update_seq, options);
                                subscription.promise.onChange(onChange);
                                success(subscription.session);
                            }
                        });
                    },
                "json");
        }
    }

    function commandHandle(code) {
        subscribe(function(session) {
            db.saveDoc({ messageType: "in", message: code, sessionId: session.id });
        });
        
        return true;
    }

    var console = $("#console").console({
        welcomeMessage: "",
        commandHandle: commandHandle
    });

    function onSend() {
        if (subscription) {
            subscription.promise.stop();
            subscription = null;
            console.reset();
        }

        subscribe(function(session) { 
            db.saveDoc({ messageType: "in", message: "", sessionId: session.id });
        });
    }

    $("#send").click(onSend);
}

$(document).ready(init);

