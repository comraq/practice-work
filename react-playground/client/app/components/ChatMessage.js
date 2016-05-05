import React from "react";

class ChatMessage extends React.Component {
  constructor(props) {
    super(props);
    this.state = {};
  }

  render() {
    return <p>{ this.props.message }</p>
  }
}

export default ChatMessage;
