var gulp = require("gulp"),
    mocha = require("gulp-mocha"),
    TEST_PATH = "./gulp_mocha_tests/";

gulp.task("test", function() {
  gulp.src(TEST_PATH + "test.js")
      .pipe(mocha())
      .on("error", function(err) {
        this.emit("end");
      });
});

gulp.task("watch", function() {
  gulp.watch(TEST_PATH + "*.js", ["test"]);
});
