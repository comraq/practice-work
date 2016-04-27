  input = input.slice(0, input.length - 1).split("\n");
  
  var result = greedHop(input);
  console.log(result);
});

function greedHop(array) {
  if (!array || array.length <= 0 || array[0] <= 0)
    return "failure";

  var i = 0;
  var end,
      currMax;
  var results = [];
  while (i < array.length) {
    if (parseInt(array[i]) <= 0)
      return "failure";

    end = parseInt(array[i]) + i;
    currMax = end;
    results.push(i);
    if (end >= array.length)
      break;

    for (var j = i + 1; j <= end; ++j) {
      var jDistance = parseInt(array[j]);
      if (j + jDistance >= currMax) {
        currMax = j + jDistance;
        i = j;
      }
    }
  }
  results.push("out");
  return results.join(", ");
}

