module.exports = function(gulp, SERV_JS_SRC_GLOB) {
  return function() {
    return gulp.watch(SERV_JS_SRC_GLOB, [ "minify-server-js" ]);
  };
};
