module.exports = function(wagner) {
  wagner.constant("BIN_PATH", "./client/bin");
  wagner.constant("JS_SRC", "./client/app/app.js");
  wagner.constant("JS_SRC_GLOB", [
                    "./client/app/**/*.js"
                  ]);
  wagner.constant("JS_DEST", "scripts.min.js");
  
  wagner.constant("CSS_SRC_GLOB", [
                    "./client/css/*.css",
                    "./client/app/**/*.css"
                  ]);
  wagner.constant("CSS_DEST", "styles.min.css");
  
  wagner.constant("CSS_VEND_SRC_GLOB", [
                    // Bootstrap
                    "./node_modules/bootstrap/dist/**/*.css"
                  ]);
  wagner.constant("CSS_VEND_DEST", "vendors.min.css");

  // Test Paths
  wagner.constant("TESTS_PATH", "./test/*.js");
  wagner.constant("TESTS_SRC_JS", [
    "./**/*.js",
    "!./node_modules/**/*.js",
    "!./gulpfile.js",
    "!./gulp/**/*.js",
    "!./test/**/*.js"
  ]);
};
