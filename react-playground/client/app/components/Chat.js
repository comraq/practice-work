import React from "react";

import MessageList from "./MessageList";
import MessageForm from "./MessageForm";
import ConnectionForm from "./ConnectionForm";

import * as MessageStore from "../messageStore";
import * as ConnectionManager from "../connection/connectionManager";

class Chat extends React.Component {
  constructor(props) {
    super(props);

    this.updateConnection = this.updateConnection.bind(this);
    this.updateMessages = this.updateMessages.bind(this);

    // Originally in getInitialState
    this.state = {
      messages: MessageStore.getMessages(),
      connected: ConnectionManager.isConnected()
    };

    // Originally in componentWillMount
    MessageStore.subscribe(this.updateMessages);
    ConnectionManager.onStatusChange(this.updateConnection);
    ConnectionManager.onMessage(MessageStore.newMessage);
  }

  componentWillUnmount() {
    MessageStore.unsubscribe(this.updateMessages);
    ConnectionManager.offStatusChange(this.updateConnection);
    ConnectionManager.offMessage(MessageStore.newMessage);
  }

  updateMessages() {
    this.setState({
      messages: MessageStore.getMessages()
    });
  }

  updateConnection() {
    this.setState({
      connected: ConnectionManager.isConnected()
    });
  }

  onSend(newMsg) {
    console.log("onSend new message:");
    console.log(newMsg);

    ConnectionManager.sendMessage(newMsg);
    MessageStore.newMessage(newMsg);
  }

  render() {
    return <div>
      <h3>Messages</h3>
      <MessageList messages={ this.state.messages } />
      <MessageForm onSend={ this.onSend } />
      <ConnectionForm connected={ this.state.connected }
        onHost={ ConnectionManager.host }
        onJoin={ ConnectionManager.join }
        />
    </div>;
  }
}

export default Chat;
