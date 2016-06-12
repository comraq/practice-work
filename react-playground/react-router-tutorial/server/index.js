import express from "express";
import morgan from "morgan";

const app = express();
const port = process.argv[2] || process.env.PORT || 80;

app.use(morgan("dev"));

app.use("/", express.static(__dirname + "/../client"));
app.get("*", (req, res) => {
  res.write("something\n");
  res.end("Thanks from ES2015 js syntax!");
});

app.listen(port);
console.log("Listening on port " + port + "...");
