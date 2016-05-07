#!/usr/bin/env node

import express from "express";
import morgan from "morgan";

const app = express();
let port = process.argv[2] || process.env.PORT || 80;

app.use(morgan("dev"));

app.get("*", (req, res) => {
  res.end("Thanks from ES2015 js syntax!");
});

app.listen(port);
console.log("Listening on port " + port + "...");
