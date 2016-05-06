import React from "react";

class CommentForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      author: "",
      text: ""
    }

    this.authorChanged = this.authorChanged.bind(this);
    this.textChanged = this.textChanged.bind(this);
    this.formSubmitted = this.formSubmitted.bind(this);
  }

  authorChanged(event) {
    this.setState({ author: event.target.value });
  }

  textChanged(event) {
    this.setState({ text: event.target.value });
  }

  formSubmitted(event) {
    event.preventDefault();

    let author = this.state.author.trim();
    let text = this.state.text.trim();

    if (!text || !author)
      return

    this.props.onCommentSubmit({ author: author, text: text });
    this.setState({ author: "", text: "" });
  }

  render() {
    return <form className="commentForm" onSubmit={ this.formSubmitted }>
        <input type="text" value={ this.state.author }
               onChange={ this.authorChanged } />
        <input type="text" value={ this.state.text }
               onChange={ this.textChanged } />
        <input type="submit" value="Post" />
      </form>;
  }
}

export default CommentForm;
