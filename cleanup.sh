#!/bin/bash
read -d '' script <<EOF
var i, docs = [];
for (i in this.rows) {
    var doc = this.rows[i].value;
    docs.push({ _id: doc._id, _rev: doc._rev, _deleted: true });
}
return { docs: docs };
EOF

url=localhost:5984
curl -d @<(curl "http://$url/tryfs/_design/app/_view/messages?endkey=\"2011-03-06\"" | jsawk "$script") -X POST -H "Content-Type: application/json" http://$url/tryfs/_bulk_docs
