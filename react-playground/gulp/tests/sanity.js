module.exports = function(gulp, plugins, TESTS_SRC_JS) {
  return function() {
    gulp.watch(TESTS_SRC_JS, [ "tests" ]);
  };
};

