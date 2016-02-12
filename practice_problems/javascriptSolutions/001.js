var twoSum = function(nums, target) {
  var obj = {};
  for (var i = 0; i < nums.length; ++i) {
    var diff = target - nums[i];
    if (obj[diff] !== undefined) {
      return [obj[diff], i + 1];
    }
    if (obj[nums[i]] === undefined) {
      obj[nums[i]] = i + 1;
    }
  }
};

(function main() {
  var arr = [4,3,2,1];
  var target = 7;

  console.log(twoSum(arr, target));
})();
