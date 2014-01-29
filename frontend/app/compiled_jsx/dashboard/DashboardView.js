/** @jsx React.DOM */

define(['React'], function(React) {

  var comp = React.createClass({displayName: 'comp',
    render: function() {
      return (
        React.DOM.div( {className:"commentBox"}, 
        " Hello, world! I am a CommentBox. "
        )
      );
    }
  });

  return comp;

});