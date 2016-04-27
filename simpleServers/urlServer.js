/**
 * A simple server taking another Url as Url params
 * and returns a shortened Url encoded by 5 random letters.
 */
var express = require("express"),
    app = express();

var db = {};
app.get("*", function(req, res) {
  var old = null
      changed = "Invalid url!";
  var url = req.url.slice(1);
  if (db[url] === undefined) {
    var result = url.match(/https?:\/\//);
    if (result != null) {
      // URL entered is valid, saves a shortened url and returns it
      shortUrl = getRandomURL(5);
      old = url;
      changed = "http://" + req.headers.host + "/" + shortUrl;
      db[shortUrl] = url; 
    }

    var reply = {
      original: old,
      shortened: changed
    };
    res.writeHead(200, {"Content-Type": "application/json"});
    res.end(JSON.stringify(reply));
  } else {
    // Saved shortened URL found, redirect accordingly
    res.redirect(db[url]);
  }
});
app.listen(7777);

// Generate a shortened URL consisting of 5 random capital letters
function getRandomURL(len) {
  len = (len === undefined)? 5 : len;
  var result = "";
  for (var i = 0; i < len; ++i)  
    result += String.fromCharCode(parseInt(Math.random() * 26) + 65);

  return result;
}
