function(head, req) {  
    start({ headers: { "Content-Type": "application/json" } });
    send("{ \"docs\": [");

    var row, comma = false;
    while (row = getRow()) {
        if (comma) {
            send(", ");
        } else {
            comma = true;
        }

        send(JSON.stringify({ _id: row.value._id, _rev: row.value._rev, _deleted: true }));
    }

    send("] }");
}
