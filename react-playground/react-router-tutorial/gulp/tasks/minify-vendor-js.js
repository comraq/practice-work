var browserify = require("browserify"),
    babelify = require("babelify"),
    buffer = require("vinyl-buffer"),
    source = require("vinyl-source-stream");

module.exports = function(gulp, plugins, vendorDeps,
                          BIN_PATH, JS_VEND_SRC, JS_VEND_DEST) {
  return function() {
    var b = browserify(JS_VEND_SRC);

    // Make dependencies in this script available to other browserified
    // scripts (such as the actual source client js)
    vendorDeps.forEach(function(dep) { b.require(dep); });

    b.transform(babelify);
    return b.bundle()
            .pipe(source("bundle.js"))
            .pipe(buffer())
            .pipe(plugins.rename(JS_VEND_DEST))
            .pipe(plugins.uglify())
            .pipe(gulp.dest(BIN_PATH));
  };
};
