var browserSync = require("browser-sync");

module.exports = function(gulp, plugins, BIN_PATH, BASE_PATH) {
  var host = plugins.util.env.host || false,
      port = plugins.util.env.port || false;

  return function() {
    var options = {};
    if (host && port)
      options.proxy = host + ":" + port;
    else {
      options.server = {
        baseDir: BASE_PATH
      };
    }
   
    var instance = browserSync.create();
    instance.init(options);

    return gulp.watch(BIN_PATH + "/**/*").on("change", instance.reload);
  };
};
