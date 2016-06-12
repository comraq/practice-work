module.exports = function(gulp, BIN_PATH, CSS_SRC_GLOB) {
  return function() {
    return gulp.watch(CSS_SRC_GLOB, [ "minify-css" ]);
  };
};
