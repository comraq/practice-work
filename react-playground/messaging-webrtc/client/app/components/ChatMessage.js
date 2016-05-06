import React from "react";

class ChatMessage extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return <p>{ this.props.message }</p>
  }
}

export default ChatMessage;
