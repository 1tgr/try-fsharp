function initSend() {
    var onSend = function() {
        $.post("exec.ashx", { code: $("#code").val() });
    }

    $("#send").click(onSend);
}

function initConsole() {
    var onData = function (data) {
        $("#console").text(data);
    }

    var onTimer = function () {
        setTimeout(onTimer, 1000);
        $.get("console.ashx", onData);
    }

    onTimer();
}

$(document).ready(initSend)
           .ready(initConsole);