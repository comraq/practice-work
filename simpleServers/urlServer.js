var express = require("express"),
    app = express();

var shortUrl = "";

app.get(shortUrl, function(req, res) {
  console.log("Redirected!");
});

app.get("*", function(req, res) {
  var url = req.url.slice(1);
  var result = url.match(/https?:\/\//),
      old = (result != null)? url : null;
  var changed;
  if (result != null) {
    shortUrl = "/" + getRandomURL(5);
    changed = "http://" + req.headers.host + shortUrl;
  } else
    changed = "Invalid url!";

  console.log(result);

  var reply = {
    original: old,
    shortened: changed
  };
  res.writeHead(200, {"Content-Type": "application/json"});
  res.end(JSON.stringify(reply));
});
app.listen(7777);

function getRandomURL(len) {
  len = (len === undefined)? 5 : len;
  var result = "";
  for (var i = 0; i < len; ++i)  
    result += String.fromCharCode(parseInt(Math.random() * 26) + 65);

  return result;
}
