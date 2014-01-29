/** @jsx React.DOM */

define(['React'], function(React) {

  var comp = React.createClass({
    render: function() {
      return (
        <div className="commentBox">
        Hello, world! I am a CommentBox.
        </div>
      );
    }
  });

  return comp;

});