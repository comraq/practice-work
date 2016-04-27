var express = require("express");
var app = express();

app.get("*", function(req, res) {
  console.log("Properties of req:");
  for (var prop in req)
    console.log(prop);
  console.log("\n\n" + req.params);
  console.log(req.query);
  console.log(req.url);

  res.end("Thanks for inquiring!");
});
app.listen(7777);
