/*
 * Note: IIFE not required due to browserify auto scoping to
 *        preventing global pollution
 */

// Include external global scripts from vendors (such as jQuery/angular)
require("../vendors/scripts");

// Include non-global scripts/libraries
import React from "react";
import ReactDOM from "react-dom";

import FilterableProductTable from "./components/FilterableProductTable";

// Rest of the app's scripts goes here
(() => {
  let dataPath = "app/data.json";
  ajax(dataPath)
    .then(data =>
      ReactDOM.render(
        <FilterableProductTable data={ JSON.parse(data) } />,
        document.getElementById("my-div")
      )
    )
    .catch(console.log.bind(console));
})();

function ajax(path) {
  return new Promise(function(resolve, reject) {
    var xmlhttp = new XMLHttpRequest();
 
    xmlhttp.onreadystatechange = function() {
      if (xmlhttp.readyState == 4) {
        if (xmlhttp.status == 200)
          resolve(xmlhttp.responseText);
        else
          reject(xmlhttp.status);
      }
    }

    xmlhttp.open("GET", path);
    xmlhttp.send();
  });
}
