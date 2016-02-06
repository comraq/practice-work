/**
 * A simple server taking a search string as Url params,
 * using Google APIs, returns a list of google images
 * searches in JSON.
 *
 * If a duplicate search query was entered, a history of past
 * searches and results are displayed.
 */
var express = require("express"),
    googleapis = require("googleapis");

var app = express(),
    customSearch = googleapis.customsearch("v1");

const CX = "015896410973065896054:wkccegel2ng";
const API_KEY = "AIzaSyDbwoNW0rqfViWLXY6AMwut1wVx2tFFbG4";

var history = {}; 
app.get("*", function(req, res) {
  var search = req.url.substring(1),
      reply = {};

  res.writeHead(200, {"Content-Type": "application/json"});
  if (history[search] === undefined) {
    history[search] = {
      date: Date(),
      query: search,
      results: []
    };
    var srchObj = {
      searchType: "image",
      cx: CX,
      q: search,
      auth: API_KEY
    };
  
    customSearch.cse.list(srchObj, function(err, resp) {
      if (err) {
        console.log('An error occured with searching', err);
        return;
      }
      // Got the response from custom search
      console.log('Result: '
                  + resp.searchInformation.formattedTotalResults);
      if (resp.items && resp.items.length > 0) {
        console.log("Sample First result: "
                    + JSON.stringify(resp.items[0]));
        reply[search] = resp.items.map(function(e, i, arr) {
          var result = {
            title: e.title,
            imgUrl: e.link,
            contextUrl: e.image.contextLink,
            thumbnail: e.image.thumbnailLink
          };
          history[search].results.push(result);
          return result;
        });
      }
      res.end(JSON.stringify(reply));
    });
  } else
    res.end(JSON.stringify(history));
});
app.listen(7777);

