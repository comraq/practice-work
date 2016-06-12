var superagent = require("superagent"),
    httpStatus = require("http-status"),
    expect = require("chai").expect;

var host = "http://localhost",
    port = 2222,
    params = "practice_work/react-playground/client";

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
