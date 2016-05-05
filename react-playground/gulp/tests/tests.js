module.exports = function(gulp, plugins, TESTS_PATH) {
  // TODO: Hardcode testSuite as "sanity" for now
  var testSuite = "sanity";

  return function() {
    var error = false;
    return gulp
      .src(TESTS_PATH) 
      .pipe(plugins.mocha({ grep: testSuite }))
      .on("error", function(err) {
        plugins.util.log(err);
        console.log("Tests in " + testSuite + " failed!");
        error = true;
      })
      .on("end", function() {
        if (!error)
          console.log("Tests in " + testSuite + " passed!");
      });
  };
};
