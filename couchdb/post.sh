#!/bin/bash
date="`date`"
curl --data "{ \"message\": \"$date\" }" --header 'Content-Type: application/json' http://www.partario.com/couchdb/tryfs
