var fs = require("fs");

function greedHop(array) {
  if (!array || array.length <= 0 || array[0] <= 0)
    return [];

  var i = 0;
  var end,
      currMax;
  var results = [];
  while (i < array.length) {
    end = parseInt(array[i]) + i;
    currMax = end;
    results.push(i);
    if (end > array.length)
      break;

    for (var j = i + 1; j <= end; ++j) {
      if (j + parseInt(array[j]) >= currMax) {
        currMax = j + parseInt(array[j]);
        i = j;
      }
    }
  }
  results.push("done");
  return results;
}

(function main() {
//  var arr = [5, 6, 0, 4, 2, 4, 1, 0, 0, 4];
  var arr = [1, 3, 5, 8, 9, 2, 6, 7, 6, 8, 9];
  console.log(arr);
  console.log(greedHop(arr));
  fileRead("CustomInput7.txt");
//  streamRead("CustomInput7.txt");
})();

function fileRead(fn) {
  fs.readFile(fn, "utf8", function(err, data) {
    if (err) throw err;

    var array = data.split("\n");
    console.log(array);
    console.log(greedHop(array));
  });
}

function streamRead(fn) {
  var rStream = fs.createReadStream(fn);
  rStream.setEncoding("utf8");
  rStream.on("data", function(chunk) {
    console.log("Chunk " + chunk);
  })
  .on("end", function() {
    console.log("Finished reading");
  });
}

