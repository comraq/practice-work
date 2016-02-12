var reverseWords = function(str) {
  str = str.replace(/ +/g, " ").trim();;
  return str.split(" ").reverse().join(" ");
};

(function main() {
  console.log(reverseWords("alskjr 234k2j    2sdkfj3  "));
})();
