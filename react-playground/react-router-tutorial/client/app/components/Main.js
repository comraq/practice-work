import React from "react";
import { Link, IndexLink } from "react-router";

import NavLink from "./NavLink";
import Home from "./Home";

class Main extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return <div>
      <h1>React Router Tutorial</h1>
      <ul role="nav">
        <li><NavLink onlyActiveOnIndex={ true } to="/">Home</NavLink></li>
        <li><NavLink to="/about">About</NavLink></li>
        <li><NavLink to="/repos">Repos</NavLink></li>
      </ul>

      { this.props.children }
    </div>;
  }
}

export default Main;
