var superagent = require("superagent"),
    httpStatus = require("http-status"),
    expect = require("chai").expect;

var host = "http://localhost",
    port = 3333,
    params = "NodeWS/gulp-setup/client";

describe("sanity: ", function() {
  describe("./index.html", function() {
    it("should respond to GET", function(done) {
      superagent
        .get(host + ":" + port + "/" + params)
        .end(function(err, res) {
          expect(res.status).to.equal(httpStatus.OK);
          done();
      });
    });
  });
});
