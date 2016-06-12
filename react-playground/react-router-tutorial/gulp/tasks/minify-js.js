module.exports = function(minifyJs) {
  return function() {
    // By default, gulp tasks pass a finish callback as the first argument
    // However, we want to manipulate this by manually calling minifyJs()
    return minifyJs();
  };
};
