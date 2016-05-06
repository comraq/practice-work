#!/usr/bin/env node
"use strict";

var _express = require("express");

var _express2 = _interopRequireDefault(_express);

var _morgan = require("morgan");

var _morgan2 = _interopRequireDefault(_morgan);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var app = (0, _express2.default)();
var port = process.argv[2] || process.env.PORT || 80;

app.use((0, _morgan2.default)("dev"));

app.get("*", function (req, res) {
  res.end("Thanks from ES2015 js syntax!");
});

app.listen(port);
console.log("Listening on port " + port + "...");