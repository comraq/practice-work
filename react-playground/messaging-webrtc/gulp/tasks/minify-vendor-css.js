module.exports = function(gulp, plugins, BIN_PATH,
                          CSS_VEND_SRC_GLOB, CSS_VEND_DEST) {

  return function() {
    return gulp.src(CSS_VEND_SRC_GLOB)
      .pipe(plugins.newer(BIN_PATH + "/" + CSS_VEND_DEST))
      .pipe(plugins.cleanCSS())
      .pipe(plugins.concat(CSS_VEND_DEST))
      .pipe(gulp.dest(BIN_PATH));
  };
};
