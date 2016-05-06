import React from "react";

import Comment from "./Comment";

class CommentList extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    let comments = this.props.data.map(comment => (
      <Comment author={ comment.author } key={ comment.id }>
        { comment.text }
      </Comment>
    ));
    return <div className="commentList">
      { comments }
    </div>;
  }
}

export default CommentList;
