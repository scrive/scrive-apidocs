/** @jsx React.DOM */

define(['React','legacy_code'],function(React) {

return React.createClass({
    propTypes: {
    },
    render: function() {
      return (
        <tr style={{"display": this.props.data.isExpanded() ? "" : "none"}}>
          {/* if */ this.props.data.isExpanded() &&
            this.props.rendering(this.props.data,this.props.index)
          }
        </tr>
      );
    }
});


});

