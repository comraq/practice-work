/**
 * A simple server taking processing the client's ip, 
 * device and language information, returning it as JSON. 
 */
var express = require("express"),
    app = express();

app.get("*", function(req, res) {
  res.writeHead(200, {"Content-Type": "application/json"});
  var reply = {
    ipaddress: req.ip.split(":").pop(),
    language: req.headers["accept-language"].split(",")[0],
    device: req.headers["user-agent"].match(/\([^\)]*\)/)[0].slice(1, -1)
  };
  res.end(JSON.stringify(reply));
});
app.listen(7777);
