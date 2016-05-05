import React from "react";
import ReactDOM from "react-dom";

import Chat from "./components/Chat";

// Rest of the app's scripts goes here
(() => {
  ReactDOM.render(
    <Chat />,
    document.getElementById("my-div")
  );
})();
