import React from "react";

import ChatMessage from "./ChatMessage";

class MessageList extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    let messages = this.props.messages.map((msg, i) => {
      return <ChatMessage key={ i }
                          message={ msg } />;
    });

    return <div>{ messages }</div>
  }
}

export default MessageList;
