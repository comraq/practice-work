module.exports = function(gulp, plugins,
                          BIN_PATH, CSS_SRC_GLOB, CSS_DEST) {
  // gulp-sourcemaps bug with NodeV6, avoid sourcemapping for now
  // https://github.com/floridoo/gulp-sourcemaps/issues/192
  var production = true; // plugins.util.env.production || false;

  return function() {
    return gulp.src(CSS_SRC_GLOB)
      .pipe((!production)?
             plugins.sourcemaps.init() : plugins.util.noop())
        .pipe(plugins.cleanCSS())
        .pipe(plugins.concat(CSS_DEST))
      .pipe((!production)?
             plugins.sourcemaps.write() : plugins.util.noop())
      .pipe(gulp.dest(BIN_PATH));
  };
};
