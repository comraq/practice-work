var watchify = require("watchify");

module.exports = function(plugins, transformedInst, minifyJs) {
  return function() {
    var watchifyInst = watchify(transformedInst);
    watchifyInst.on("update", function() {
      return minifyJs(watchifyInst);
    });
    watchifyInst.on("log", function() {
      var cyan = plugins.util.colors.cyan;
      var magenta = plugins.util.colors.magenta;

      var arg = arguments[0].replace(/\((.*)\)/, "(" + magenta("$1") + ")");
      var msg = "Finished '" + cyan("watchify-js(bundle)") + "' " + arg;
      return plugins.util.log(msg);
    });

    return minifyJs(watchifyInst);
  };
};
