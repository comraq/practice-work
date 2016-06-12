var gulp = require("gulp"),
    wagner = require("wagner-core");

wagner.factory("gulp", function() {
  return gulp;
});

// Gulp initilization configuration
require("./gulp/config")(wagner);

// Path constants
require("./gulp/paths")(wagner);

// Gulp tasks list
wagner.invoke(require("./gulp/tasks-list"));
