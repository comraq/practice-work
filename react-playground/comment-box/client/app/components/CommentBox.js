import React from "react";

import CommentList from "./CommentList";
import CommentForm from "./CommentForm";

class CommentBox extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      data: []
    };

    this.getComments = this.getComments.bind(this);
    this.onCommentSubmit = this.onCommentSubmit.bind(this);
  }

  componentDidMount() {
    this.getComments();
    setInterval(this.getComments, this.props.pollInterval);
  }

  getComments() {
    $.ajax({
      url: this.props.url,
      dataType: "json",
      cache: false,
      success: data => this.setState({ data: data }),
      error: (xhr, status, err) =>
               console.error(this.props.url, status, err.toString())
    });
  }

  onCommentSubmit(comment) {
    let comments = this.state.data;

    comment.id = Date.now();
    let newComments = comments.concat([comment]);
    this.setState({ data: newComments });

    $.ajax({
      url: this.props.url,
      dataType: "json",
      type: "POST",
      data: comment,
      success: data => this.setState({ data: data }),
      error: (xhr, status, err) => {
        this.setState({ data: comments });
        console.error(this.props.url, status, err.toString());
      }
    });
  }

  render() {
    return <div className="commentBox">
      <h1>Comments in this CommentBox Project!</h1>
      <CommentList data={ this.state.data }/>
      <CommentForm onCommentSubmit={ this.onCommentSubmit }/>
    </div>;
  }
}

export default CommentBox;
