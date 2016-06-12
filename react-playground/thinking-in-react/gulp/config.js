module.exports = function(wagner) {
  wagner.factory("plugins", function() {
    return require("gulp-load-plugins")({
      rename: { "gulp-clean-css": "cleanCSS" }
    });
  });

  wagner.factory("getTask", function() {
    return getTask;
  });

  wagner.factory("getTest", function() {
    return getTest;
  });

  // Flags
  wagner.constant("production",
                  require("gulp-util").env.production || false);

  // Get task utility functions
  var gulpTasksDir = "./tasks/";
  function getTask(task) {
    return wagner.invoke(require(gulpTasksDir + task));
  }

  var gulpTestsDir = "./tests/";
  function getTest(test) {
    return wagner.invoke(require(gulpTestsDir + test));
  }
};
