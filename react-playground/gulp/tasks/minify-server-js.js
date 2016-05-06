module.exports = function(gulp, plugins,
                          SERV_BASE_PATH, SERV_JS_SRC, SERV_JS_DEST) {
  return function() {
    return gulp.src(SERV_JS_SRC)
      .pipe(plugins.babel())
      .pipe(plugins.rename(SERV_JS_DEST))
      .pipe(gulp.dest(SERV_BASE_PATH));
  };
};
