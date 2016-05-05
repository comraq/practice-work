module.exports = function(gulp, getTask, getTest) {
  // Build Tasks
  gulp.task("watch-css", getTask("watch-css"));
  gulp.task("watchify-js", getTask("watchify-js"));
  gulp.task("minify-css", getTask("minify-css"));
  gulp.task("minify-vendor-css", getTask("minify-vendor-css"));

  // Test Tasks
  gulp.task("tests", getTest("tests"));
  gulp.task("sanity", getTest("sanity"));
};
