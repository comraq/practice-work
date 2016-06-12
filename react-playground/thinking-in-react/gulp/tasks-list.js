module.exports = function(gulp, getTask, getTest) {
  // Build Tasks
  gulp.task("watch-css", getTask("watch-css"));
  gulp.task("watchify-js", getTask("watchify-js"));
  gulp.task("minify-css", getTask("minify-css"));
  gulp.task("minify-vendor-css", getTask("minify-vendor-css"));

  // Server Build Tasks
  gulp.task("watch-server-js", getTask("watch-server-js"));
  gulp.task("minify-server-js", getTask("minify-server-js"));

  // Test Tasks
  gulp.task("tests", getTest("tests"));
  gulp.task("sanity", getTest("sanity"));
};
