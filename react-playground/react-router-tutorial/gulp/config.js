module.exports = function(wagner) {
  wagner.factory("plugins", function() {
    return require("gulp-load-plugins")({
      rename: { "gulp-clean-css": "cleanCSS" }
    });
  });

  // List of external vendor dependencies
  wagner.factory("vendorDeps", function() {
    return [
      "jquery",
      "bootstrap",
      "react",
      "react-dom",
      "react-router"
    ];
  });

  wagner.factory("getTask", function() {
    return getTask;
  });

  wagner.factory("getTest", function() {
    return getTest;
  });

  // A transformed browserify instance with all options/pluggins set
  wagner.factory("transformedInst", function(JS_SRC, vendorDeps) {
    var browserify = require("browserify"),
        babelify = require("babelify");

    var options = {
      entries: [ JS_SRC ],
      cache: {},
      packageCache: {},
      poll: 100
    };

    var b = browserify(options);

    // Externalize the vendor require/imports from source code
    // and require them from a separate bundle
    vendorDeps.forEach(function(dep) { b.external(dep); });

    return b.transform(babelify);
  });

  // Client js build function, also === "minify-js" gulp task
  // Needed as a dependency due to watchify
  wagner.factory("minifyJs", function(gulp, plugins,
                                      JS_DEST, BIN_PATH, transformedInst) {
    var buffer = require("vinyl-buffer"),
        source = require("vinyl-source-stream");

    return function(browserifyInst) {
      var instance = browserifyInst || transformedInst,
          production = plugins.util.env.production || false;

      return instance
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
  });

  // Log the status of production flag
  wagner.invoke(function(plugins) {
    var production = plugins.util.env.production || false;

    var colour = (production)?
                   plugins.util.colors.bgGreen
                 :
                   plugins.util.colors.bgYellow;
  
    plugins.util.log("Production", colour(production));
  });

  // Get task utility functions
  var gulpTasksDir = "./tasks/";
  function getTask(task) {
    return wagner.invoke(require(gulpTasksDir + task));
  }

  var gulpTestsDir = "./tests/";
  function getTest(test) {
    return wagner.invoke(require(gulpTestsDir + test));
  }
};
