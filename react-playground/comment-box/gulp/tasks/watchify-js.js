var browserify = require("browserify"),
    watchify = require("watchify"),
    babelify = require("babelify"),
    buffer = require("vinyl-buffer"),
    source = require("vinyl-source-stream");

module.exports = function(gulp, plugins, production,
                          BIN_PATH, JS_SRC, JS_DEST) {
  var browserifyOpts = {
    entries: [ JS_SRC ],
    debug: true
  };

  // Add customer browserify options on top of default watchify options
  var allOptions = {};
  for (var prop in watchify.args)
    allOptions[prop] = watchify.args[prop];

  for (var prop in browserifyOpts)
    allOptions[prop] = browserifyOpts[prop];
  
  var b = watchify(browserify(allOptions))
            .transform(babelify);

  b.on("update", bundleFunc);
  b.on("log", function() {
    var cyan = plugins.util.colors.cyan;
    var magenta = plugins.util.colors.magenta;

    var arg = arguments[0].replace(/\((.*)\)/, "(" + magenta("$1") + ")");
    var msg = "Finished '" + cyan("watchify-js(native)") + "' " + arg;
    return plugins.util.log(msg);
  });

  function bundleFunc() {
    return b
      .bundle()
      .on("error", function(err) {
        plugins.util.log(err);
        this.emit("end");
      })
      .pipe(source("bundle.js"))
      .pipe(buffer())
      .pipe(plugins.rename(JS_DEST))
      .pipe((!production)?
                plugins.sourcemaps.init({ loadMaps: true })
             :
                plugins.util.noop())
        .pipe(plugins.uglify())
      .pipe((!production)?
              plugins.sourcemaps.write() : plugins.util.noop())
      .pipe(gulp.dest(BIN_PATH));
  };

  return bundleFunc;
};
