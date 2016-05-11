import React from "react";
import ReactDOM from "react-dom";
import { Router, Route, hashHistory, IndexRoute } from "react-router";

import Home from "./components/Home";
import Main from "./components/Main";
import About from "./components/About";
import Repos from "./components/Repos";
import Repo from "./components/Repo";

const app = document.getElementById("app");
ReactDOM.render(
  <Router history={ hashHistory }>
    <Route path="/" component={ Main }>
      <IndexRoute component={ Home } />

      <Route path="/about" component={ About } />
      <Route path="/repos" component={ Repos }>
        <Route path="/repo/:userName/:repoName" component={ Repo } />
      </Route>
    </Route>
  </Router>,
  app
);
