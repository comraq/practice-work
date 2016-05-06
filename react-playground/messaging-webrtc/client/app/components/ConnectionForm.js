import React from "react";

class ConnectionForm extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return <div>
      { (this.props.connected)? "Connected": "Not Connected" }
      <button onClick={ this.props.onHost }>Host</button>
      <button onClick={ this.props.onJoin }>Join</button>
    </div>;
  }
}

export default ConnectionForm;
