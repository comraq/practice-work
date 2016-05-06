#!/usr/bin/env node
"use strict";

var _ws = require("ws");

var server = new _ws.Server({ port: 3210 });
new Promise(function (resolve, reject) {
  server.on("connection", function (socket) {
    socket.on("message", resolve);
  });
}).then(function (msg) {
  console.log("got msg:");
  console.log(msg);

  server.clients.forEach(function (other) {
    if (other === socket) return;

    other.send(msg);
  });
});