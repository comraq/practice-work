module.exports = function(gulp, getTask, getTest, minifyJs, plugins) {
  gulp.task("build-all", [
                           "minify-vendor-js",
                           "minify-vendor-css",
                           "minify-css",
                           "minify-js",
                           "minify-server-js"
                         ]);
  gulp.task("watch-all", [
                           "minify-vendor-js",
                           "minify-css",
                           "watch-css",
                           "watchify-js",
                           "minify-server-js",
                           "watch-server-js"
                         ]);
  gulp.task("watch-reload", [ "watch-all" ], getTask("watch-reload"));

  // Sub Tasks (Client)
  gulp.task("watch-css", getTask("watch-css"));
  gulp.task("minify-css", getTask("minify-css"));
  gulp.task("watchify-js", getTask("watchify-js"));
  gulp.task("minify-js", getTask("minify-js"));

  gulp.task("minify-vendor-js", getTask("minify-vendor-js"));
  gulp.task("minify-vendor-css", getTask("minify-vendor-css"));

  // Sub Tasks (Server)
  gulp.task("watch-server-js", getTask("watch-server-js"));
  gulp.task("minify-server-js", getTask("minify-server-js"));

  // Test Tasks
  gulp.task("tests", getTest("tests"));
  gulp.task("sanity", getTest("sanity"));
};
