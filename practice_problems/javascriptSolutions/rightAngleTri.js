function printTri(n, curr) {
  if (!curr)  
    curr = 0;

  if (n < curr)
    return;


  for (var i = 0; i < n; ++i) {
    if (i < n - curr)
      process.stdout.write(" ");
    else
      process.stdout.write("X");
  }
  console.log("");
  return printTri(n, curr + 1);
}

(function main() {
  printTri(13);
})();
