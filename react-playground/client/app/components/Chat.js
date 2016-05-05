import React from "react";

import ChatMessage from "./ChatMessage";

class Chat extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      text: "",
      messages: []
    };
  }

  render() {
    return <div>
      <h3>Messages</h3>
      <div>{ this.state.messages }</div>
      <form onSubmit={ this.submit }>
        <input type="text" placeholder="Enter any message here"
          onChange={ this.updateInput } value={ this.state.text }/>
        <input type="submit" value="Submit Message" />
      </form>
    </div>;
  }

  submit = (event) => {
    event.preventDefault();

    let newMsg = <ChatMessage key={ this.state.messages.length }
                              message={ this.state.text } />
    this.setState({
      text: "",
      messages: this.state.messages.concat(newMsg)
    });
  }

  updateInput = (event) => {
    this.setState({ text: event.target.value });
  }
}

export default Chat;
