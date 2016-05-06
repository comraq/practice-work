import React from "react";

class MessageForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      input: ""
    };
  }

  render() {
    return <form onSubmit={ this.submit }>
      <input value={ this.state.input } onChange={ this.updateInput }
        type="text" />
      <input type="submit" value="Submit Message" />
    </form>;
  }

  submit = (event) => {
    event.preventDefault();

    this.props.onSend(this.state.input);
    this.setState({
      input: ""
    });
  };

  updateInput = (event) => {
    this.setState({ input: event.target.value });
  };
}

export default MessageForm;
