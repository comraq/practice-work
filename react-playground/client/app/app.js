import React from "react";
import ReactDOM from "react-dom";

import Chat from "./components/Chat";

import HostConnection from "./connection/hostConnection";

// Rest of the app's scripts goes here
(() => {
  ReactDOM.render(
    <Chat />,
    document.getElementById("my-div")
  );
})();
