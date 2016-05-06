module.exports = function(gulp, plugins,
                          BIN_PATH, CSS_SRC_GLOB, production) {
  return function() {
    var colour = (production)?
                   plugins.util.colors.bgGreen
                 :
                   plugins.util.colors.bgYellow;
  
    plugins.util.log("Production", colour(production));
    gulp.watch(CSS_SRC_GLOB, [ "minify-css" ]);
  };
};

