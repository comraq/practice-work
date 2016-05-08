module.exports = function(gulp, plugins,
                          SERV_BASE_PATH, SERV_JS_SRC_GLOB) {
  return function() {
    return gulp.src(SERV_JS_SRC_GLOB)
      .pipe(plugins.babel())
      .pipe(gulp.dest(SERV_BASE_PATH))
  };
};
