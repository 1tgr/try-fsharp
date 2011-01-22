function(data) {
  $.log(data)
  var p;
  return {
    items : data.rows.map(function(r) {
      p = (r.value && r.value.profile) || {};
      p.message = r.value && r.value.message;
      p.sessionId = r.value && r.value.sessionId;
      p.type = r.value && r.value.type;
      return p;
    })
  }
};
