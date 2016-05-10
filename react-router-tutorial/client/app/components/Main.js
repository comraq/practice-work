import React from "react";
import { Link } from "react-router";

class Main extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return <div>
      <h1>React Router Tutorial</h1>
      <ul role="nav">
        <li><Link to="/about">About</Link></li>
        <li><Link to="/repos">Repos</Link></li>
      </ul>

      { this.props.children }
    </div>;
  }
}

export default Main;
