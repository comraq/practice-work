/**
 * A simple server taking a date string as Url params
 * and returns the time in unix timestamp and regular format.
 */
var express = require("express"),
    app = express();

app.get("*", function(req, res) {
  res.writeHead(200, {"Content-Type": "application/json"});

  var time = new Date(req.url.split("%20").join(" ").slice(1));
  var reply = {
    unix: (time == "Invalid Date")? null : time.getTime(),
    natural: (time == "Invalid Date")? null : time.toDateString()
  };
  res.end(JSON.stringify(reply));
});
app.listen(7777);
