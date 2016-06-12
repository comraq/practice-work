/*
 * Note: IIFE not required due to browserify auto scoping to
 *        preventing global pollution
 */

// Include external global scripts from vendors (such as jQuery/angular)
require("../vendors/scripts");

// Include non-global scripts/libraries
import React from "react";
import ReactDOM from "react-dom";

import CommentBox from "./components/CommentBox";

let serverUrl = "/api/comments";

// Rest of the app's scripts goes here
(() => {
  ReactDOM.render(
    <CommentBox url={ serverUrl } pollInterval={ 2000 }/>,
    document.getElementById("my-div")
  );
})();
