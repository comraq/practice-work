import React from "react";
import ReactDOM from "react-dom";
import { Router, Route, hashHistory } from "react-router";

import Main from "./components/Main";
import About from "./components/About";
import Repos from "./components/Repos";

const app = document.getElementById("app");
ReactDOM.render(
  <Router history={ hashHistory }>
    <Route path="/" component={ Main }>
      <Route path="/about" component={ About } />
      <Route path="/repos" component={ Repos } />
    </Route>
  </Router>,
  app
);
